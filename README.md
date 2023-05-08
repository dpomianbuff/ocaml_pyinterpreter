# Python interpreter written in OCaml
This is a mini Python code interpreter written in OCaml

## Supported features
1. Statements
    - FunctionDef
    - Assign
    - AugAssign
    - Return
    - Expr
    - If
    - For
    - While
2. Expressions
    - Constant
    - Name
    - BinOp
    - BoolOp
    - Compare
    - Call
    - List
    - Tuple

## Project Dependencies
This is a [Dune](https://dune.build/) project and it has two dependencies

**[pyre-ast](https://github.com/grievejia/pyre-ast)** - This library is used to parse python files into AST. The resulting AST mimics pretty
closely the [official Python AST](https://docs.python.org/3/library/ast.html).

**[sexplib](https://github.com/janestreet/sexplib)** - This library contains functionality for parsing and pretty-printing s-expressions. It helped me to print the AST structure and facilitated the debugging process

## How to run
The application has two flavors:
1. Run tests
2. Interpret a new file

To run the tests, which cover the aforementioned supported features, is as simple as running the following shell command:
```shell
dune exec --profile release pyint test
```

To interpret a python file, run the following shell command:
```shell
dune exec --profile release pyint interpret path/to/your/python/file.py
```

For convenience, in `bin/testfiles` folder, I have included a simple test file that you could run:
```shell
dune exec --profile release pyint interpret bin/testfiles/test_sum.py
```

The output produced by the interpreter is basically the evaluation environment.


## Future work
- Expand the interpreter to work for all the Python AST constructs
- Fine tune it for performance