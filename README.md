# plc-test
This repository aims to test some of the interpreters featured in the textbook Programming Language Concepts by Peter Sestoft.

This project uses FsCheck in order to apply Property Based Testing (PBT) to the interpreters.

Information about the book and all book files can be found [here](https://www.itu.dk/people/sestoft/plc).

## micro-ML
Micro-ML is a subset of the ML functional language family for which Sestoft creates an interpreter.

### Type preservation
Our intention is to apply PBT to the interpreter in order to be able to consider the interpreter to be type preserving, while not proving it formally. An interpreter is type preserving when the type that can be determined from a non-evaluated expression _e_ is equal to the type determined from the expression evaluated from _e_.

Unfortunately, the interpreter contained in the [TypedFun](https://www.itu.dk/people/sestoft/plc/typedfun.zip) module is not type preserving, since its output is an `int` in each and every case:
```fsharp
let rec eval (e : tyexpr) (env : value env) : int = ...
```

What we can do, however, is create an upgraded version of the interpreter, which has a tyexpr as both input and output:
```fsharp
let rec eval (e : tyexpr) (env : value env) : (e : tyexpr) = ...
```
then we can use the same type evaluator provided by the book to check if the evaluated type is equal to the non-evaluated one.

### Generator
