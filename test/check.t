Diagnostics from the command line.

  $ export C0LS_LIB_DIR="$PWD/../c0lib"

A well-formed program with contracts, loops and a library:

  $ cat > fib.c0 <<'EOF'
  > #use <conio>
  > 
  > /* Computes the nth Fibonacci number */
  > int fib(int n)
  > //@requires n >= 0;
  > //@ensures \result >= 0;
  > {
  >   if (n < 2) return n;
  >   int a = 0;
  >   int b = 1;
  >   for (int i = 2; i <= n; i++)
  >   //@loop_invariant i >= 2;
  >   {
  >     int c = a + b;
  >     a = b;
  >     b = c;
  >   }
  >   return b;
  > }
  > 
  > int main() {
  >   printf("fib(10) = %d\n", fib(10));
  >   return 0;
  > }
  > EOF
  $ c0ls check fib.c0

Type errors, flow errors, and style warnings:

  $ cat > bad.c0 <<'EOF'
  > int f(int x) {
  >   int y = true;
  >   if (x) { return 1; }
  >   z = 3;
  >   return "hello";
  > }
  > 
  > bool g(int n) {
  >   if (n > 0) { return true; } else { return false; }
  > }
  > 
  > int h() {
  >   int w;
  >   return w;
  > }
  > EOF
  $ c0ls check bad.c0
  bad.c0:2:11: error: expected to find a 'int', but this expression has an incompatible type: 'bool'
  bad.c0:3:7: error: expected to find a 'bool', but this expression has an incompatible type: 'int'
  bad.c0:4:3: error: variable z not declared
  bad.c0:5:10: error: expected to find a 'int', but this expression has an incompatible type: 'string'
  bad.c0:14:10: error: local 'w' used without necessarily being defined
  bad.c0:9:3: warning: unnecessary if statement
  
      Hint: consider replacing this if statement with 'return <loop guard>;'
  [1]


Structs, typedefs and pointers:

  $ cat > structs.c0 <<'EOF'
  > typedef struct point pt;
  > struct point { int x; int y; };
  > 
  > int sum(pt* p)
  > //@requires p != NULL;
  > { return p->x + p->y; }
  > 
  > int main() {
  >   pt* p = alloc(pt);
  >   p->x = 3;
  >   int bad = p.x;
  >   return sum(p);
  > }
  > EOF
  $ c0ls check structs.c0
  structs.c0:11:13: error: subject of access '.x' is a pointer, not a struct
  [1]

Statement-position-only constructs used as expressions:

  $ cat > stmtexpr.c0 <<'EOF'
  > int main() {
  >   int x = 0;
  >   int y = (x = 3) + 1;
  >   return y;
  > }
  > EOF
  $ c0ls check stmtexpr.c0
  stmtexpr.c0:3:12: error: Assignment 'x = e2' must be used as a statement; it is used as an expression here.
  [1]

Language levels: bool is not part of L1.

  $ cat > lab.l1 <<'EOF'
  > int main() {
  >   bool b = true;
  >   return 0;
  > }
  > EOF
  $ c0ls check lab.l1
  lab.l1:2:3: error: type 'bool' not a part of the language 'L1'
  [1]

break/continue are C1-only:

  $ cat > brk.c0 <<'EOF'
  > int main() {
  >   while (true) { break; }
  >   return 0;
  > }
  > EOF
  $ c0ls check brk.c0
  brk.c0:2:18: error: 'break' not a part of the language 'C0'
  [1]

C1 function pointers, casts and void*:

  $ cat > funptr.c1 <<'EOF'
  > typedef int binop(int x, int y);
  > int add(int x, int y) { return x + y; }
  > 
  > int apply(binop* f, int a, int b)
  > //@requires f != NULL;
  > { return (*f)(a, b); }
  > 
  > int main() {
  >   binop* f = &add;
  >   void* v = (void*)f;
  >   binop* g = (binop*)v;
  >   return apply(g, 3, 4);
  > }
  > EOF
  $ c0ls check funptr.c1

Functions declared and used but never defined:

  $ cat > undef.c0 <<'EOF'
  > int missing();
  > int main() { return missing(); }
  > EOF
  $ c0ls check undef.c0
  undef.c0:2:21: error: function missing was declared but never defined
  undef.c0:1:5: error: function missing was declared but never defined
  [1]

Bad numeric and character literals:

  $ cat > lits.c0 <<'EOF'
  > int main() {
  >   int a = 0x;
  >   return 0;
  > }
  > EOF
  $ c0ls check lits.c0
  lits.c0:2:11: error: Invalid hex constant: 0x
      Hex constants must only have the characters '0123456789abcdefABCDEF'
  [1]

printf format checking (via #use <conio>):

  $ cat > fmt.c0 <<'EOF'
  > #use <conio>
  > int main() {
  >   printf("%d %s\n", 1);
  >   return 0;
  > }
  > EOF
  $ c0ls check fmt.c0
  fmt.c0:3:3: error: found 2 format specifiers, but got 1 arguments
  [1]

An unknown library:

  $ cat > nolib.c0 <<'EOF'
  > #use <nosuchlib>
  > int main() { return 0; }
  > EOF
  $ c0ls check nolib.c0
  nolib.c0:1:1: error: library 'nosuchlib' not found
  [1]

Multi-file projects via README.txt: util.c0 is listed before main.c0, so
its functions are visible from main.c0.

  $ mkdir proj
  $ cat > proj/util.c0 <<'EOF'
  > int twice(int x) { return 2 * x; }
  > EOF
  $ cat > proj/main.c0 <<'EOF'
  > int main() { return twice(21); }
  > EOF
  $ cat > proj/README.txt <<'EOF'
  >    % cc0 -d util.c0 main.c0
  > EOF
  $ c0ls check proj/main.c0

A type error in a dependency:

  $ cat > proj/util.c0 <<'EOF'
  > int twice(int x) { return true; }
  > EOF
  $ c0ls check proj/main.c0
  proj/main.c0:1:1: error: Failed to typecheck 'util.c0'. Please fix that file first before editing this one
  [1]
