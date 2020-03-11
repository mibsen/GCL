#  A parser for GCL
We have implemented a parser for GCL Which accepts or rejects programs and builds AST for them. 

The solution is built using FsLexYacc.

## How to build
1. Compile the lexer using fslex.exe
`./FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ./GCLLexer.fsl --unicode`

2. Compile the parser using fsyacc.exe
`./FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ./GCLParser.fsp --module GCLParser`

## How to run
`fsi.exe GCL.fsx <Path to source file>` 

Prints the AST for the parsed source code or an error explaining where the error happened.

Example:
`fsi.exe GCL.fsx ./test/cases/case0.txt` 



## Tests
Multiple test cases are placed in the `./test/cases` folder.


