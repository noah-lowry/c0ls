An end-to-end LSP session over stdio: initialize, open a document, then
hover / go-to-definition / document symbols / completion / signature help,
and finally shutdown+exit.

  $ export C0LS_LIB_DIR="$PWD/../c0lib"
  $ cat > demo.c0 <<'EOF'
  > struct point {
  >   int x;
  >   int y;
  > };
  > 
  > /* Manhattan norm of p */
  > int norm(struct point* p)
  > //@requires p != NULL;
  > {
  >   return p->x + p->y;
  > }
  > 
  > int main() {
  >   struct point* q = alloc(struct point);
  >   q->x = 3;
  >   int n = norm(q);
  >   return n;
  > }
  > EOF

Hover/definition at `norm` on line 15; completion after `q->` on line 14;
signature help inside `norm(q)`:

  $ ./lsp_client.exe "$(command -v c0ls)" demo.c0 15 11 14 5 15 16
  CAPS: hoverProvider=true definitionProvider=true documentSymbolProvider=true positionEncoding="utf-8"
  CAPS2: completion=yes signatureHelp=yes
  DIAGNOSTICS: 0
  HOVER: ```c0\nint norm(struct point* p)\n//@requires p != NULL;\n```\nManhattan norm of p
  DEFINITION: demo.c0 line 6
  SYMBOLS: struct point, norm, main
  COMPLETION (2): x, y
  SIGNATURE: int norm(struct point* p)
  SERVER EXIT: 0

Diagnostics are published for a file with errors:

  $ cat > broken.c0 <<'EOF'
  > int f() {
  >   return true;
  > }
  > EOF
  $ ./lsp_client.exe "$(command -v c0ls)" broken.c0 0 5 0 5 0 5
  CAPS: hoverProvider=true definitionProvider=true documentSymbolProvider=true positionEncoding="utf-8"
  CAPS2: completion=yes signatureHelp=yes
  DIAGNOSTICS: 1
    line 1: expected to find a 'int', but this expression has an incompatible type: 'bool'
  HOVER: ```c0\nint f()\n```\n
  DEFINITION: broken.c0 line 0
  SYMBOLS: f
  COMPLETION (5): f, assert, error, alloc, alloc_array
  SIGNATURE: none
  SERVER EXIT: 0
