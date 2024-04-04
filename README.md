# Cesame
Cesame: A C-like programming language that features native support for first-class func- tions. Cesame also supports non-primitive types such as String, Array, and Struct. Cesame abstracts away pointers and stores most data on the heap, except for primitives and refer- ences to objects, similar to Java. The compiler is written in OCaml with C libraries and targets LLVM IR.

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

## Work Remaining
- **Timeline:** As soon as possible

- Structures
- Functions
- Statements
    - if Statements
    - for Statements
- Codegen

## Functionalities

## Scanner

## Parser

## Semantic Checker

## IR Generator

# Testing Workflow