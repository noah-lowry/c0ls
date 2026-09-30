disclaimer: this is a port of the official C0 LSP for vscode that works as a general purpose language server in other text editors. there should be no guarantees of the quality or reliability of this software in the future, and exists only to provide a little extra convenience when doing cmu coursework.

# c0ls — a language server for C0

`c0ls` is an editor-agnostic [LSP](https://microsoft.github.io/language-server-protocol/)
server for **C0** (and C1), the safe C subset taught in CMU's 15-122, written in
OCaml. It works with any LSP-capable editor: Neovim, Vim, Helix, Emacs, Kate,
VS Code, and more.

Features:

- **Diagnostics** as you type: syntax errors, type errors, contract errors,
  "does not return along every path", "used without being defined",
  `printf`/`format` format-string checking, plus a few style warnings
- **Hover**: types of variables and fields, function prototypes with their
  `//@requires` / `//@ensures` contracts and doc comments
- **Go to definition** for functions, typedefs, structs, struct fields,
  locals, and `#use` targets
- **Completion**: locals in scope, functions, typedefs, structs, struct
  fields after `.` / `->`, and contract keywords after `//@`
- **Signature help** while typing call arguments
- **Document symbols** (outline)
- **`#use <lib>`** support with the standard library headers bundled
  (`conio`, `string`, `parse`, `file`, `args`, `img`, `rand`, `util`, ...)
- **Multi-file projects** via the 15-122 `README.txt` convention: a line like
  `% cc0 -d util.c0 main.c0` makes every file listed before the open one a
  dependency (globs like `*.c0` work too; `project.txt` is also recognized)
- **`.o0` / `.o1` object files** (tar archives produced by newcc0) as
  dependencies, including interface-section warnings
- Language levels by file extension: `.l1`–`.l4`, `.c0`/`.h0`, `.c1`/`.h1`
  (e.g. `bool` is rejected in `.l1` files, `break` outside C1, …)

There is also a batch mode, handy in CI or as a quick checker:

```console
$ c0ls check foo.c0
foo.c0:4:3: error: variable z not declared
```

## Building and installing

Requires OCaml ≥ 4.14 (the default OCaml on CMU's Andrew Linux servers works)
with `dune`, `linol`, `linol-lwt` (which pull in `lsp`, `jsonrpc`, `lwt`,
`yojson`):

```console
$ opam install dune linol linol-lwt
$ dune build
$ dune install          # installs the c0ls binary and the c0lib headers
```

For development you can run the server without installing: `dune exec c0ls`.
If the bundled library headers are not found automatically, point
`C0LS_LIB_DIR` at a directory containing the `.h0` files.

Run the test suite with `dune test`.

## Editor setup

### Neovim (0.11+)

```lua
vim.filetype.add { extension = { c0 = 'c0', h0 = 'c0', c1 = 'c0', h1 = 'c0' } }

vim.lsp.config['c0ls'] = {
  cmd = { 'c0ls' },
  filetypes = { 'c0' },
  root_markers = { 'README.txt', 'project.txt', '.git' },
}
vim.lsp.enable 'c0ls'
```

### Neovim (older, or with nvim-lspconfig)

```lua
vim.filetype.add { extension = { c0 = 'c0', h0 = 'c0', c1 = 'c0', h1 = 'c0' } }

local configs = require 'lspconfig.configs'
if not configs.c0ls then
  configs.c0ls = {
    default_config = {
      cmd = { 'c0ls' },
      filetypes = { 'c0' },
      root_dir = require('lspconfig.util').root_pattern('README.txt', 'project.txt', '.git'),
      single_file_support = true,
    },
  }
end
require('lspconfig').c0ls.setup {}
```

### Vim with [vim-lsp](https://github.com/prabirshrestha/vim-lsp)

```vim
autocmd BufNewFile,BufRead *.c0,*.h0,*.c1,*.h1 setfiletype c0
if executable('c0ls')
  autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'c0ls',
        \ 'cmd': {server_info->['c0ls']},
        \ 'allowlist': ['c0'],
        \ })
endif
```

### Helix

In `~/.config/helix/languages.toml`:

```toml
[language-server.c0ls]
command = "c0ls"

[[language]]
name = "c0"
scope = "source.c0"
file-types = ["c0", "h0", "c1", "h1"]
comment-token = "//"
roots = ["README.txt", "project.txt"]
language-servers = ["c0ls"]
```

### Emacs (eglot)

```elisp
(define-derived-mode c0-mode c-mode "C0")
(add-to-list 'auto-mode-alist '("\\.[ch][01]\\'" . c0-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(c0-mode . ("c0ls"))))
```

## Notes

- Diagnostics, messages, and typechecking behavior closely follow the
  [C0 VSCode extension](https://github.com/CalLavicka/c0-vscode-extension)'s
  language server, of which this is an OCaml rewrite; the language itself is
  specified in the [C0 reference](https://c0.cs.cmu.edu/docs/c0-reference.pdf).
- When a file has no `README.txt`/`project.txt`, it is checked on its own;
  functions from sibling files will show as undeclared. Add a
  `% cc0 file1.c0 file2.c0 ...` line to a `README.txt` next to (or one
  directory above) your sources to check multi-file projects.

## Project layout

- `lib/lexer.ml`, `lib/parser.ml` — hand-written lexer (with contract
  annotation modes and typedef feedback) and recursive-descent parser that
  enforces the L1…C1 language levels
- `lib/{genv,typerel,exprcheck,stmtcheck,flow,progcheck,style}.ml` — the
  typechecker: bidirectional expression checking, statement checking,
  definite-initialization and return-path flow analysis, style lints
- `lib/validate.ml`, `lib/project.ml`, `lib/tar.ml` — whole-program checking:
  `#use` libraries, `README.txt` dependencies, object-file archives
- `lib/{ast_search,completions,print}.ml` — position→AST search, completion
  contexts, pretty-printing for hover
- `lib/server.ml` — the LSP server (linol); `lib/cli.ml` — `c0ls check`
- `c0lib/` — the bundled standard library headers
- `test/` — cram tests, including a scripted end-to-end LSP session
