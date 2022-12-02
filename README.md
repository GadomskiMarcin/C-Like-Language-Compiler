# C Like Language Complier - University project

## Description
Compiler takes as input C-Like language, parses it and compiles to LLVM.  
Creates and creates binary files from LLVM code

## Use
The programs are compiled using the ``make`` command
`latc` and `latc_llvm` executables appear (Same executables, but due to technical requirements both are included)

## Parsing and files
Parser generated with bnfc ``bnfc --haskell --functor -m Latte.cf``
(Happy and Alex libraries are needed to generate, but there is no bnfc on students)
Whereas bnfc was returning some grammar issues (BNFC v2.9.3)

### Tests
The tests come from the archive `lattests201003.tgz`
The program rejects all bad tests from the `bad` file and accepts and compiles (with correct answers) the files from `good`

## Results and commentary
### Accept
When accepting, the frontend returns "OK"

### Reject
If rejected, the frontend only returns an error. Syntactic using a parser.
Semantic using the frontend, the error is in the form of place + content + id (Exceptions such as no main or no id for 1 + "someString")  

### Common Intermediate Language
The compiler generates an intermediate language with instructions from `Instr`, then after generation and after optimization the instructions are printed (Except `IAss` and `IDeclare` as these are mainly helping instructions)

### Registers, expressions and phi
The compiler optimizes simple expressions, generates registers when it overwrites a value through an expression or function call, or when it needs it to be passed on, as in the comparison

### Runtime vs Semantic
The program finds basic runtime errors like division by zero and discards them

### Optimizations
Fully working `lsce` and `gsce` with a few exceptions (i.e. passing value to phi on change)
E.g.
```
%t1 = someValue
%t2 = someValue * 3
%t3 = someValue  + 2
br %cond %L1 %L2
L1:
%t11 = someValue * 3
br L3
L2:
%t22 = someValue + 2
L3
%res = phi [%t11 %L1, %t22 %L2]  

```
