# featherweight-C

Featherweight C, Executable Semantics: Parser, Type Checker, and
Abstract Machine

This little language includes several kinds of values: integer,
Booleans, pointers, and function pointers. Regarding control-flow, it
includes goto and function calls.

The parser is implemented using the flex and bison parser generator
tools.

* `syntax.l` the lexer specification
* `syntax.y` the grammar

The parser generates an abstract syntax tree.

* `ast.h` structure definitions for the AST and declarations of
  functions for creating and printing ASTs.
  
* `ast.cc` definitions of functions for creating and printing ASTs.

The type checker makes sure that the AST is a valid program.
It prints an error and exits if the AST is invalid.

* `typechecker.h`
* `typechecker.cc`

