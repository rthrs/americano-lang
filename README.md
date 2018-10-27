# Americano language
Monadic interpreter for toy programming language written in Haskell.

Americano is imperative and strongly typed with no implicit type casting. It has C-like syntax and features:
- `int`, `bool`, `string` and `void` primitive types
- built-in arrays and dictionaries types (combining them also possible)
- integer arithmetic and side-effects assign operators
- comparision for both primitive and complex types (python way)
- `string` concatenation
- `if`, `while`, `break`, `continue` instructions
- nested, recursive functions with static binding
- two kinds of function arguments passing - primitives by value, complex types by reference (python way)
- assiging value `v` to non existing index `i` increases size of the array up to `i` and fills newly allocated space with `v`. 
- runtime exceptions handling
- not yet implemented static type checker :)

Language grammar is defined in bnfc format.

## Usage
```
  make
  ./interpreter [--help] [FILES...]
```
`./interpreter` without arguments runs interactive shell.
Americano programming examples are placed under `good` directory.

