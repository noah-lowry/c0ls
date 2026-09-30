(* A minimal growable array. The stdlib gained Dynarray in OCaml 5.2, but we
   support 4.14 (the default on CMU's Andrew machines), so we roll our own. *)

type 'a t = { mutable data : 'a array; mutable len : int }

let create () : 'a t = { data = [||]; len = 0 }

let length (v : 'a t) : int = v.len

let get (v : 'a t) (i : int) : 'a =
  if i < 0 || i >= v.len then invalid_arg "Vec.get";
  v.data.(i)

let add_last (v : 'a t) (x : 'a) : unit =
  if v.len = Array.length v.data then begin
    let cap = max 8 (2 * Array.length v.data) in
    let data = Array.make cap x in
    Array.blit v.data 0 data 0 v.len;
    v.data <- data
  end;
  v.data.(v.len) <- x;
  v.len <- v.len + 1

(* Removes the last element if any. The slot keeps a stale reference until it
   is overwritten, which is fine for our small, short-lived vectors. *)
let pop_last (v : 'a t) : unit = if v.len > 0 then v.len <- v.len - 1

let to_list (v : 'a t) : 'a list = List.init v.len (fun i -> v.data.(i))
