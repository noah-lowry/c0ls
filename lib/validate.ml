(* Whole-document validation: resolves dependencies (README.txt/project.txt),
   loads #use'd libraries and files, parses everything in order, and runs the
   typechecker. This is the engine behind both diagnostics publishing in the
   LSP server and the `c0ls check` command line. *)

open Ast

(* ---------------- URIs ---------------- *)

let uri_of_path (path : string) : string =
  let abs =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  (* normalize away "." and ".." segments *)
  let parts = String.split_on_char '/' abs in
  let stack =
    List.fold_left
      (fun stack part ->
        match part with
        | "" | "." -> stack
        | ".." -> ( match stack with _ :: rest -> rest | [] -> [])
        | p -> p :: stack)
      [] parts
  in
  "file:///" ^ String.concat "/" (List.rev stack)

let percent_decode (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    (if s.[!i] = '%' && !i + 2 < n then begin
       match int_of_string_opt ("0x" ^ String.sub s (!i + 1) 2) with
       | Some code ->
         Buffer.add_char buf (Char.chr code);
         i := !i + 3
       | None ->
         Buffer.add_char buf s.[!i];
         incr i
     end
     else begin
       Buffer.add_char buf s.[!i];
       incr i
     end)
  done;
  Buffer.contents buf

let path_of_uri (uri : string) : string =
  if Util.starts_with ~prefix:"file://" uri then
    percent_decode (String.sub uri 7 (String.length uri - 7))
  else uri

(* ---------------- source files ---------------- *)

type source_file = {
  sf_key : string; (* URI (or virtual URI for object file entries) *)
  sf_display : string; (* how the file is named in messages *)
  sf_contents : string;
  sf_object : string option; (* URI of the containing .o0/.o1 archive *)
}

let mk_diag ?loc severity msg : Err.diagnostic =
  { Err.d_loc = loc; d_msg = msg; d_severity = severity }

let line_span (line : int) : Loc.span =
  Loc.mk { Loc.line; col = 1 } { Loc.line; col = 1 }

(* cache of parsed library headers: name -> (decls, typedef names) *)
let libcache : (string, decl list * string list) Hashtbl.t = Hashtbl.create 16

(* Stamp the source URI on all top-level declaration spans. *)
let stamp_sources (uri : string) (decls : decl list) : unit =
  List.iter
    (fun d ->
      match decl_loc d with
      | Some l when l.Loc.source = None -> l.Loc.source <- Some uri
      | _ -> ())
    decls

(* Mark functions of an object-file source that are outside its "Interface"
   section as local to the archive (calling them elsewhere is a warning). *)
let mark_non_interface (file : source_file) (decls : decl list) : unit =
  let archive = Option.value file.sf_object ~default:file.sf_key in
  let lines = String.split_on_char '\n' file.sf_contents in
  let contains line word =
    let ll = String.length line and lw = String.length word in
    let rec go i = i + lw <= ll && (String.sub line i lw = word || go (i + 1)) in
    lw > 0 && go 0
  in
  (* List.find_index only exists from OCaml 5.1 on *)
  let find_index pred lst =
    let rec go i = function
      | [] -> None
      | x :: rest -> if pred x then Some i else go (i + 1) rest
    in
    go 0 lst
  in
  let interface_start = find_index (fun line -> contains line "Interface") lines in
  match interface_start with
  | None ->
    (* no interface section: everything is public *)
    ()
  | Some start0 ->
    let end0 =
      let rec find i = function
        | [] -> List.length lines - 1
        | line :: rest -> if i > start0 && contains line "End" then i else find (i + 1) rest
      in
      find 0 lines
    in
    (* positions in decls are 1-indexed *)
    let istart = start0 + 1 and iend = end0 + 1 in
    let interface_funcs =
      List.filter_map
        (fun d ->
          match d with
          | FunDecl f -> (
            match f.floc with
            | Some l when istart < l.Loc.start_p.line && l.Loc.end_p.line < iend ->
              Some f.fname.name
            | _ -> None)
          | _ -> None)
        decls
    in
    List.iter
      (fun d ->
        match d with
        | FunDecl f when not (List.mem f.fname.name interface_funcs) ->
          f.is_local_to <- Some archive
        | _ -> ())
      decls

(* ---------------- parsing with #use ---------------- *)

type parse_ctx = {
  genv : Genv.t;
  mutable typeids : string list;
  overlay : string -> string option; (* unsaved editor contents by URI *)
  mutable notices : string list;
}

let read_source (pcx : parse_ctx) (uri : string) : string option =
  match pcx.overlay uri with
  | Some contents -> Some contents
  | None -> ( try Some (Tar.read_file (path_of_uri uri)) with _ -> None)

(* Parse one source file, loading its #use dependencies first. Returns the
   declarations belonging to the program (from this file and #use'd files)
   plus non-fatal warnings, or error diagnostics. *)
let rec parse_document (pcx : parse_ctx) (file : source_file) :
    (decl list * Err.diagnostic list, Err.diagnostic list) result =
  let diags = ref [] in
  let extra_decls = ref [] in
  let lang = Option.value (Lang.of_filename file.sf_key) ~default:Lang.C1 in

  (* Scan for #use lines first: libraries and files must be loaded before the
     body is parsed so their typedefs are known to the lexer. *)
  let lines = String.split_on_char '\n' file.sf_contents in
  List.iteri
    (fun i line ->
      let line = String.trim line in
      if Util.starts_with ~prefix:"#use" line then
        match Parser.parse_use_pragma line with
        | Some (`Lib libname) -> load_library pcx file diags (i + 1) libname
        | Some (`File used_name) ->
          diags :=
            mk_diag ~loc:(line_span (i + 1)) Err.Warning
              (Printf.sprintf
                 "'#use \"%s\"' syntax is deprecated and will be removed in the future" used_name)
            :: !diags;
          load_used_file pcx file diags extra_decls (i + 1) used_name
        | None -> ())
    lines;

  (* Now parse the file itself. *)
  let parser = Parser.create ~lang ~typeids:pcx.typeids file.sf_contents in
  let { Parser.decls; diagnostics; typedefs } = Parser.parse_program parser in
  stamp_sources file.sf_key decls;
  let errors_present =
    List.exists (fun (d : Err.diagnostic) -> d.Err.d_severity = Err.Error) (!diags @ diagnostics)
  in
  if errors_present then Error (List.rev !diags @ diagnostics)
  else begin
    pcx.typeids <- typedefs;
    (* anything left over is a warning (e.g. the #use deprecation notice) *)
    Ok (List.rev !extra_decls @ decls, List.rev !diags @ diagnostics)
  end

and load_library (pcx : parse_ctx) (file : source_file) (diags : Err.diagnostic list ref)
    (line : int) (libname : string) : unit =
  ignore file;
  if not (Genv.SSet.mem libname pcx.genv.Genv.libs_loaded) then begin
    pcx.genv.Genv.libs_loaded <- Genv.SSet.add libname pcx.genv.Genv.libs_loaded;
    let cached = Hashtbl.find_opt libcache libname in
    match cached with
    | Some (libdecls, lib_typedefs) ->
      pcx.typeids <- lib_typedefs @ pcx.typeids;
      add_library_decls pcx libdecls
    | None -> (
      match Project.lib_header_path libname with
      | None ->
        diags :=
          mk_diag ~loc:(line_span line) Err.Error (Printf.sprintf "library '%s' not found" libname)
          :: !diags
      | Some path -> (
        let lib_uri = uri_of_path path in
        match Tar.read_file path with
        | contents -> (
          let lib_file =
            { sf_key = lib_uri; sf_display = libname ^ ".h0"; sf_contents = contents; sf_object = None }
          in
          let saved_typeids = pcx.typeids in
          match parse_document pcx lib_file with
          | Ok (libdecls, _warnings) ->
            stamp_sources lib_uri libdecls;
            let lib_typedefs =
              List.filter (fun t -> not (List.mem t saved_typeids)) pcx.typeids
            in
            Hashtbl.replace libcache libname (libdecls, lib_typedefs);
            add_library_decls pcx libdecls
          | Error _ ->
            diags :=
              mk_diag ~loc:(line_span line) Err.Error
                (Printf.sprintf "error reading library '%s'" libname)
              :: !diags)
        | exception _ ->
          diags :=
            mk_diag ~loc:(line_span line) Err.Error (Printf.sprintf "library '%s' not found" libname)
            :: !diags))
  end

and add_library_decls (pcx : parse_ctx) (libdecls : decl list) : unit =
  List.iter
    (fun d ->
      Genv.add_decl ~library:true pcx.genv d;
      (* typedefs from a cached library still need to reach the lexer *)
      match d with
      | TypeDef { td_def; _ } ->
        if not (List.mem td_def.p_id.name pcx.typeids) then
          pcx.typeids <- td_def.p_id.name :: pcx.typeids
      | FunTypeDef f ->
        if not (List.mem f.fname.name pcx.typeids) then pcx.typeids <- f.fname.name :: pcx.typeids
      | _ -> ())
    libdecls

and load_used_file (pcx : parse_ctx) (file : source_file) (diags : Err.diagnostic list ref)
    (extra_decls : decl list ref) (line : int) (used_name : string) : unit =
  let base_dir = Filename.dirname (path_of_uri file.sf_key) in
  let used_path =
    if Filename.is_relative used_name then Filename.concat base_dir used_name else used_name
  in
  let used_uri = uri_of_path used_path in
  if not (Genv.SSet.mem used_uri pcx.genv.Genv.files_loaded) then begin
    (* add before parsing to prevent #use cycles *)
    pcx.genv.Genv.files_loaded <- Genv.SSet.add used_uri pcx.genv.Genv.files_loaded;
    match read_source pcx used_uri with
    | None ->
      diags :=
        mk_diag ~loc:(line_span line) Err.Error (Printf.sprintf "couldn't find %s" used_name)
        :: !diags
    | Some contents -> (
      let used_file =
        {
          sf_key = used_uri;
          sf_display = used_name;
          sf_contents = contents;
          sf_object = None;
        }
      in
      match parse_document pcx used_file with
      | Ok (used_decls, _warnings) ->
        stamp_sources used_uri used_decls;
        extra_decls := !extra_decls @ used_decls
      | Error _ ->
        diags :=
          mk_diag ~loc:(line_span line) Err.Error
            (Printf.sprintf
               "failed to typecheck %s. Code completion and other features will not be available"
               used_name)
          :: !diags)
  end

(* ---------------- dependency discovery ---------------- *)

(* The dependency files (in parse order) for [uri], from README.txt or
   project.txt. Missing dependencies produce notices. *)
let dependency_files (pcx_overlay : string -> string option) ?(workspace_root : string option)
    (uri : string) : source_file list * string list =
  let notices = ref [] in
  let files = ref [] in
  (match Project.find_dependencies ?workspace_root (path_of_uri uri) with
  | None -> ()
  | Some { Project.dep_paths; _ } ->
    List.iter
      (fun dep_path ->
        let dep_uri = uri_of_path dep_path in
        if dep_uri <> uri then begin
          if not (Sys.file_exists dep_path) then
            notices :=
              Printf.sprintf
                "Dependency %s not found and will be ignored. Diagnostics and code completion might be incorrect"
                (Filename.basename dep_path)
              :: !notices
          else if Lang.is_object_file dep_path then begin
            match Tar.read_archive dep_path with
            | Some entries ->
              List.iter
                (fun (name, contents) ->
                  if Lang.of_filename name <> None then
                    files :=
                      {
                        sf_key = dep_uri ^ "/" ^ name;
                        sf_display = Filename.basename dep_path ^ "/" ^ name;
                        sf_contents = contents;
                        sf_object = Some dep_uri;
                      }
                      :: !files)
                entries
            | None ->
              notices :=
                Printf.sprintf "Could not read object file %s; it will be ignored."
                  (Filename.basename dep_path)
                :: !notices
          end
          else begin
            let contents =
              match pcx_overlay dep_uri with
              | Some c -> Some c
              | None -> ( try Some (Tar.read_file dep_path) with _ -> None)
            in
            match contents with
            | Some sf_contents ->
              files :=
                {
                  sf_key = dep_uri;
                  sf_display = Filename.basename dep_path;
                  sf_contents;
                  sf_object = None;
                }
                :: !files
            | None ->
              notices :=
                Printf.sprintf
                  "Dependency %s could not be read and will be ignored."
                  (Filename.basename dep_path)
                :: !notices
          end
        end)
      dep_paths);
  (List.rev !files, List.rev !notices)

(* ---------------- whole-document check ---------------- *)

type check_result = {
  diagnostics : Err.diagnostic list; (* for the requested document *)
  genv : Genv.t option; (* available even when there are type errors *)
  notices : string list;
}

let check_document ?(overlay : string -> string option = fun _ -> None)
    ?(workspace_root : string option) ~(uri : string) ~(contents : string) () : check_result =
  let genv = Genv.create () in
  let pcx = { genv; typeids = []; overlay; notices = [] } in
  let dep_files, dep_notices = dependency_files overlay ?workspace_root uri in
  let all_decls = ref [] in
  let failed = ref None in
  (* dependencies first *)
  List.iter
    (fun dep ->
      if !failed = None && not (Genv.SSet.mem dep.sf_key genv.Genv.files_loaded) then begin
        genv.Genv.files_loaded <- Genv.SSet.add dep.sf_key genv.Genv.files_loaded;
        match parse_document pcx dep with
        | Ok (decls, _warnings) ->
          stamp_sources dep.sf_key decls;
          if dep.sf_object <> None then mark_non_interface dep decls;
          all_decls := !all_decls @ decls
        | Error _ ->
          failed :=
            Some
              [
                mk_diag Err.Error
                  (Printf.sprintf
                     "Syntax errors found in '%s'.\nCode completion and other features will not be available"
                     dep.sf_display);
              ]
      end)
    dep_files;
  match !failed with
  | Some diags -> { diagnostics = diags; genv = None; notices = dep_notices }
  | None -> (
    (* the document itself *)
    genv.Genv.files_loaded <- Genv.SSet.add uri genv.Genv.files_loaded;
    let file =
      {
        sf_key = uri;
        sf_display = !Util.display_path uri;
        sf_contents = contents;
        sf_object = None;
      }
    in
    match parse_document pcx file with
    | Error diags -> { diagnostics = diags; genv = None; notices = dep_notices }
    | Ok (decls, parse_warnings) ->
      stamp_sources uri decls;
      let all = !all_decls @ decls in
      let style_warnings = Style.check_program all in
      let cx = { Exprcheck.genv; source_file = Some uri } in
      let tc = Progcheck.check_program cx all in
      (* if a dependency fails to typecheck, tell the user to fix it first *)
      let cross_file_error =
        List.find_opt
          (fun (te : Err.terror) ->
            te.Err.severity = Err.Error
            &&
            match te.Err.tloc with
            | Some l -> ( match l.Loc.source with Some src -> src <> uri | None -> false)
            | None -> false)
          tc.Progcheck.errors
      in
      match cross_file_error with
      | Some te ->
        let src = match te.Err.tloc with Some l -> l.Loc.source | None -> None in
        {
          diagnostics =
            [
              mk_diag Err.Error
                (Printf.sprintf
                   "Failed to typecheck '%s'. Please fix that file first before editing this one"
                   (match src with Some s -> !Util.display_path s | None -> "a dependency"));
            ];
          genv = Some genv;
          notices = dep_notices;
        }
      | None ->
        let for_this_file (te : Err.terror) : bool =
          match te.Err.tloc with
          | Some l -> ( match l.Loc.source with Some src -> src = uri | None -> true)
          | None -> true
        in
        let to_diag (te : Err.terror) : Err.diagnostic =
          { Err.d_loc = te.Err.tloc; d_msg = te.Err.msg; d_severity = te.Err.severity }
        in
        let diagnostics =
          parse_warnings
          @ List.map to_diag (List.filter for_this_file (tc.Progcheck.errors @ style_warnings))
        in
        { diagnostics; genv = Some genv; notices = dep_notices })
