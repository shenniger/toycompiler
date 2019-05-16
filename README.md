This is a toy project I created mostly for learning about
compiler/transpiler design, computer architecture, and LLVM.

## Overview
This is an attempt at bridging the two worlds of C/C++-like systems programming
languages and Common Lisp/Scheme-like languages. The idea is that it should be
possible to mix both concepts seamlessly within one program by defining a flexible,
macro-friendly S-expression-based grammar and add additional syntactic sugar to make
it look like C while similarly building a core language that is very much like C
(except with a few more modern features like lambda/closure support and
advanced introspection capabilities) and make it extensible using a macro system
which can be used to create Lisp-like functions and a list type.

At the moment, I am at the point where the Lisp-like syntax and the C-like
core language are almost finished (although still poorly tested), but I have not
started implementing the C syntax in terms of S-expressions (see below for ideas
on how to do that) or implementing Lisp's type system in terms of C's.

See the TODO file for a detailed list of implemented and planned language features.

The language compiles to either C code or LLVM IR.

## Source files
* **main.c**: Error formatting, memory allocator, driver.
* **reader.c**: List functions such as an S-expression reader.
* **parser.c**: Grammar, but combined with a type system, control structures etc.
* **middle.c**: Macro system.
* **back\_c.c**: C transpiler.
* **back\_llvm.cpp**: LLVM backend.

## Compiling
Just type `make`. The Makefile is non-standard, but should be self-explanatory. If
you do not have LLVM and/or a C++ compiler, you can choose to build only the
C transpiler by using `make test_c`.

Tip: You can use `make report` to see which compiler and LLVM version is currently
being used.

## Usage example
With LLVM: `./test_llvm test2.crp | clang -xir - -o print_file_test`

With C: `./test_c test2.crp | gcc -xc - -o print_file_test`

Then `./print_file_test` should print the first characters of its own sourcecode.

## Code example
```
; This example should compile with the compiler in its current state.

; Declare a prototype for libc's `puts` function.
(funproto puts (((ptr const char) str)) i32)

; Define the `main` function with a call to `puts`. The string literal
; has, at the moment, the type of (ptr const char) (like in C), although
; I think this will change in the future.
(defun main () i32 (
  (puts "Hello, world.")
 0))
```

## A crazy idea about syntax
What if S-expressions could be written not just with parentheses but with
other characters as well? Can a syntax allow for the same flexibility as
S-expressions while looking less intimidating to those used to modern programming
languages?

I gave it a try:
```
// Declare a prototype for libc's `puts` function.
funproto puts (ptr const char: str;) i32;

// Define the `main` function with a call to `puts`. The string literal
// has, at the moment, the type of (ptr const char) (like in C), although
// I think this will change in the future.
defun main () i32 {
  puts "Hello, world.";
  0
}
```
Translate this to S-exprs using the following rules:
* Replace `a b c { d e f }` with `(a b c (d e f))`.
* Replace `a b c : d e f` with `(a b c) d e f`.
* Replace `a b c;` with `(a b c)`
(You will also need to change the comment syntax.)
