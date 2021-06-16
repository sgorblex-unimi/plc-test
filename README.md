# plc-test
This repository aims to test part of the code featured in the textbook _Programming Language Concepts_ by Peter Sestoft. In particular, the project makes use of [FsCheck](https://github.com/fscheck/FsCheck) in order to apply Property Based Testing (PBT) to said code.

Information about the book and all book files can be found [here](https://www.itu.dk/people/sestoft/plc).



## Micro-ML
Micro-ML is a subset of the ML functional language family for which Sestoft creates an abstract syntax interpreter, a lexer and a parser.


### Type preservation
An interpreter is type preserving when the type that can be determined from a non-evaluated expression _e_ is equal to the type determined from the expression evaluated from _e_.

Our intention is to apply PBT to the abstract syntax interpreter in order to be able to consider it type preserving to a certain extent, while not proving it formally.

Unfortunately, the interpreter contained in the [TypedFun](https://www.itu.dk/people/sestoft/plc/typedfun.zip) module is not type preserving, since its output is an `int` in each and every case:
```fsharp
let rec eval (e : tyexpr) (env : value env) : int = ...
```
The falseness of the thesis can in fact be proved with ease with the following counterexample:
```fsharp
> let e = eval (CstB true) [] ;;
val e : int = 1
```

What we can do, however, is create an upgraded version of the interpreter, which has a tyexpr as both input and output, and evaluates to typed constants:
```fsharp
let rec eval (e : tyexpr) (env : value env) : (e : tyexpr) = ...
```
We can then use the same type evaluator provided by the book to check for a certain expression if the evaluated type is equal to the non-evaluated one.

Said typed interpreter can be found in `TypedEval.fs`.


### Generator
The file `GenMML.fs` contains a custom `typexpr` generator. This generator generates _sound_ Micro-ML programs of a given [size](https://fscheck.github.io/FsCheck//TestData.html#The-size-of-test-data), meaning that all generated programs are able to be evaluated with no errors. The generator is also _complete_, meaning that all possible programs can be generated with non-zero probability, with some exceptions:
- integer constants are chosen in a range of -100 to 100. Not only there is no need to generate programs with huge numbers (which can still be built from smaller ones), but we also don't want the integer constant generator to be affected by the program's size and therefore by the depth of the AST (Abstract Syntax Tree) node in which the constant will appear;
- function and variable names are generated as "fx" and "vx" respectively, where x is a serial number, growing with each nested declaration. This is acceptable as different variable or function names do not affect the interpretation of the program and it improves readability and at the same time avoids ambiguous variable names (numbers or whitespaces);
- recursive functions cannot be generated, as the function declaration environment doesn't contain the name of the function itself. This choice is due to the possibility of infinite recursion and has therefore been avoided.

With said exceptions, the generator is _sound_ and _complete_, meaning it is _correct_. The _sound_ status is furthermore tested with FsCheck in that every generated program has to be evaluated.


### Testing
The file `GenMML.fs` overrides the default generator for `tyexpr`s and configures in the `config` variable a test of 10000 growing Micro-ML programs. The predicate `typePreservation` receives the generated program and checks type preservation, i.e. it determines the type of the program, then it evaluates it and checks if the type of the evaluated constant is equal to the predetermined type.

The function `test()` is just a shortcut for launching a type preservation test:
```fsharp
let test() = Check.One (config, typePreservation)
```


## Bibliography and sources
- Programming Language Concepts, Peter Sestoft, Springer, 2017
- Property-Based Testing Abstract Machine, Francesco Komauli, Università degli Studi di Milano, 2017
- Professors C. Fiorentini and A. Momigliano's Declarative Programming course @ Università degli Studi di Milano, 2021
