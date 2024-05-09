# Cesame
Cesame: A C-like programming language that features native support for first-class functions. Cesame also supports non-primitive types such as String, Array, and Struct. Cesame abstracts away pointers and stores most data on the heap, except for primitives and references to objects, similar to Java. The compiler is written in OCaml with C libraries and targets LLVM IR.

## High-lights
* Python-like program structure: Instead of starting with main(), we implement our program as a list of statements, and running the programming is just running through statements from top to bottom.
* Better control flow: Added support for if-elif-else statement and for loop.
* Add support for Struct, which declares an aggregated type (struct name and only struct name should start with a capitalized letter).
* Implemented new and delete to allocate/deallocate objects on the heap.
* Implemented built-in types Array (essentially a linked list) and a String (close to C++ library), using linked C/C++ library.
* TODO 1: socket
* TODO 2: first-class function.
* TODO 3: Matrix (contiguously stored memory, with some fancy algo in C).

## Testing
```
make

# To print out ast
./cesame -a ./test/example.csm
./cesame -a ./test/hello-world.csm

# To print out sast
./cesame -s ./test/example.csm
./cesame -s ./test/hello-world.csm

# To print out the generated LLVM IR
./cesame -l ./test/example.csm
./cesame -l ./test/hello-world.csm

# To check and print out the generated LLVM IR (default)
./cesame -c ./test/example.csm
./cesame -c ./test/hello-world.csm
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
- Floating Point (by Yunjia)
- if-elif-else (by Teng)
- for loop (by Teng)
- Restructure the program (we don't want to use main() as the entrance to the program) (by Teng and Yunjia)
- Restructure sast (by Qian)
- Shortcut for declaration and definition on the same line (by Teng and Yunjia)
- Structs (by Teng and Yunjia)
- new and delete (by Teng and Yunjia)
- Semantics for struct (by Teng and Yunjia)

## Work Remaining
- **Timeline:**
    - Before Apr 10th: Complete scanner, parser, and semantic checker.
    - After Apr 10th: Codegen.
- TODO:
    - First-class Functions (implemented as function pointers, can be kept global for now. Keep both first-class and normal function for now)
    - Build in functions for String and Array
