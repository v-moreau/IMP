# An IMP interpreter

This is a small implementation in OCaml of IMP, a language of while programs, following the book [The Formal Semantics of Programming Languages: An Introduction](https://mitpress.mit.edu/9780262731034/the-formal-semantics-of-programming-languages/) by Glynn Winskel.

The IMP language is a simple language with boolean and integer expressions, integer variables and the `if ... then ... else` and `while ... do ...` control flow statements. The formal definition of the syntax is in the `com` type, in the [syntax.ml](imp/lib/syntax.ml) file. Here is, for instance, the code in the [divisionBy2.imp](imp/examples/divisionBy2.imp) file:
```
while 2 <= in do {
    out := out + 1;
    in := in - 2
}
```

This projects uses the [dune](https://dune.build/) utility and the [ocamllex](https://v2.ocaml.org/manual/lexyacc.html) and [menhir](http://gallium.inria.fr/~fpottier/menhir/) OCaml packages. The conversion of an IMP code to an abstract syntax tree is done in the [lexer.mll](imp/lib/lexer.mll) and [parser.mly](imp/lib/parser.mly) files. The following commands build the project and execute the interpreter:
```
cd imp
dune build
dune exec <file> [in]
```

The executable takes two arguments: the first one is the IMP file to execute and the second one is the value of the `in` variable with which the execution should start (`0` by default). If the program stops, then the content of the variable `out` is sent to standard output.
