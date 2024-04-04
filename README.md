# Cesame
Cesame: A C-like programming language that features native support for first-class functions. Cesame also supports non-primitive types such as String, Array, and Struct. Cesame abstracts away pointers and stores most data on the heap, except for primitives and references to objects, similar to Java. The compiler is written in OCaml with C libraries and targets LLVM IR.

## Testing
```
cd ./src
make

# To print out ast
./cesame -a ../test/example.csm
./cesame -a ../test/hello-world.csm

# To print out sast
./cesame -s ../test/example.csm
./cesame -s ../test/hello-world.csm
```

## Work Completed
- Makefile (by Qian)
- Char (by Yunjia)
- String (Qian)
    - Added support for multiple-line strings by ignoring leading spaces between quotes
    - Implemented detection for reaching the end of the file while reading the string, throwing an error in such cases.
- Comments (by Yunjia)
    - Added General Comments
    - Added Line Comments
- Operator (by Yunjia)
    - Implemented Unary Operators: !
    - Implemented Binary Operators: *, /, %, ++, --
- Array (by Qian)
- if-elif-else (by Teng)

## Work Remaining
- **Timeline:**
    - Before Apr 10th: Complete scanner, parser, and semantic checker.
    - After Apr 10th: Codegen.
- TODO:
    - Restructure the program (we don't want to use main() as the entrance to the program)
    - Shortcut for declaration and definition on the same line
    - Structures
    - First-class Functions
    - Statements
        - for Statements


## Functionalities

## Scanner

## Parser

## Semantic Checker

## IR Generator

# Testing Workflow
