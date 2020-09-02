# featherweight-C

Featherweight C, Executable Semantics: Parser, Type Checker, and
Abstract Machine

This tiny C-like language includes several kinds of values: integer,
Booleans, pointers, and function pointers. Regarding control-flow, it
includes if statements, goto, and function calls.

The parser is implemented using the flex and bison parser generator
tools.

* `syntax.l` the lexer specification
* `syntax.y` the grammar

The parser translates program text into an abstract syntax tree (AST).

* `ast.h` includes structure definitions for the AST and function
  declarations for creating and printing ASTs.
* `ast.cc` contains the function definitions for creating and printing
  ASTs.

The type checker defines what it means for an AST to be a valid
program. The type checker prints an error and exits if the AST is
invalid.

* `typechecker.h`
* `typechecker.cc`

The parser and type checker together specify the static semantics
of Featherweight C.

The dynamic semantics of Featherweight C is specified by an abstract
machine. Abstract machines have several positive characteristics that
make them good for specification:

* abstract machines operate on the AST of the program
  (and not some lower-level representation such as bytecode)
  so they directly connect the program to its behavior
  
* abstract machines can easily handle language features with complex
  control-flow, such as goto, exceptions, coroutines, and even
  first-class continuations.
  
The one down-side of abstract machines is that they are not as simple
as a definitional interpreter (a recursive function that interprets
the program), but its more difficult to handle complex control flow in
a definitional interpreter.

* `interp.h` declares the `interp_program` function.
* `interp.cc` implements `interp_program` function using an
  abstract machine, as described below.

The abstract machine implements a state-transition system.  The state
is defined by the `State` structure, which includes three components:
the procedure call stack, the heap, and the function definitions.  The
`step` function updates the state by executing a little bit of the
program. The `step` function is called repeatedly to execute the
entire program.

An implementation of Featherweight C (such as a compiler) must be
observationally equivalent to this abstract machine. The notion of
observation is different for each language, and can include things
like input and output. Featherweight C is such a simple language that
the only thing that is observable is the final result, an integer.  So
an implementation of Featherweight C must produce the same final
result as the one produces by the abstract machine. In particular, an
implementation does **not** have to mimic each step of the abstract
machine and does not have to use the same kinds of data structures to
store the state of the program.

A procedure call frame, defined by the `Frame` structure, includes a
pointer to the function being called, the environment that maps
variables to their addresses, and a to-do list of actions.  Each
action corresponds to an expression or statement in the program.  The
`Act` structure represents an action.  An action often spawns other
actions that needs to be completed first and afterwards uses their
results to complete its action. To keep track of this process, each
action includes a position field `pos` that stores an integer that
starts at `-1` and increments as the action makes progress.  For
example, suppose the action associated with an addition expression
`e1 + e2` as at the top of the to-do list:

    (e1 + e2) [-1] :: ...

When this action kicks off (in the `step_exp` function), it increments
`pos` to `0` and pushes `e1` onto the to-do list, so the top of the
todo list now looks like:

    e1 [-1] :: (e1 + e2) [0] :: ...

Skipping over the processing of `e1`, it eventually turns into
an integer value `n1`:

    n1 :: (e1 + e2) [0]

Because there is a value at the top of the to-do list, the `step`
function invokes `handle_value` which then dispatches on the next
action on the to-do list, in this case the addition. The addition
action spawns an action for subexpression `e2`, increments
`pos` to `1`, and remembers `n1`.

    e2 [-1] :: (e1 + e2) [1](n1) :: ...

Skipping over the processing of `e2`, it eventually turns into
an integer value `n2`:

    n2 :: (e1 + e2) [1](n1) :: ...

Again the `step` function invokes `handle_value` and dispatches to the
addition action which performs the arithmetic and pushes the result on
the to-do list.  Let `n3` be the sum of `n1` and `n2`.

    n3 :: ...

The heap is an array of values. It is used not only for `malloc` but
also to store function arguments and local variables.  A pointer is
simply an index into the array.  The `malloc` expression causes the
heap to grow (at the end) and returns the index of the last slot.  The
dereference expression returns the nth value of the heap, as specified
by the dereferenced pointer. The assignment operation stores the value
of the right-hand side into the heap at the index specified by the
left-hand side lvalue.

As you might expect, function calls push a new frame on the stack and
the `return` statement pops a frame off the stack. The parameter
passing semantics is call-by-value, so the machine applies `copy_val`
to the incoming arguments and the outgoing return value.  Also, the
machine is careful to kill the parameters and local variables when the
function call is complete.

The handling of the `goto` statement deserves some explanation.  It
overwrites the todo list of the frame with a new one computed by the
function `goto_label`. This function searches through the body of the
current frame, looking for the target label. So the first parameter of
`goto_label` is the target label and the second parameter is the
current statement that it is searching.  Also, while searching,
`goto_label` accumulates a todo list of statements that come after the
one it is currently searching. The third parameter is for the
accumulated todo list. Once `goto_label` finds a statement that is
labeled with the target label, it adds that statement to the front of
the accumulated todo list and returns it.

The `examples/` subdirectory includes some Featherweight C programs.
