#  A parser for GCL
We have implemented a parser for GCL Which accepts or rejects programs and builds AST for them. 

The solution is built using FsLexYacc.

## How to build
1. Compile the lexer using fslex.exe
`./FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ./GCLLexer.fsl --unicode`

2. Compile the parser using fsyacc.exe
`./FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ./GCLParser.fsp --module GCLParser`

## How to run the parser
`fsi.exe GCL.fsx <Path to source file>` 

Prints the AST for the parsed source code or an error explaining where the error happened.

Example:
`fsi.exe GCL.fsx ./test/cases/case0.txt` 

## How to run the compiler
`fsi.exe Compiler.fsx <Path to source file> <true or false to construct deterministic graph (false is default)>` 

Prints the graph in the textual graphviz format.

Example:
`fsi.exe Compiler.fsx ./test/cases/case0.txt true`

This will print a deterministic version of the program graph

## How to run the interpreter
`fsi.exe Interpreter.fsx <Path to source file> <Path to file with initial variables> <true or false to construct deterministic graph (false is default)>`

This will print whether or not the program terminated, the node the program ended in and the memory of the program at the time of stoppage. While there is an option for deterministic and non-deterministic PG the interpreter can only deal with deterministic PG's. An example output would be:

`status: terminated`

`Node: qFinal`

`x: 13`

`y: 7`

`z: 0`

The file containing the initial variables should follow the same syntax as the initialization of variables in http://www.formalmethods.dk/fm4fun. For example:

`x = 0, y = 0, z = 0, A = [1,2,3,4]`

## Tests
Multiple test cases are placed in the `./test/cases` folder.


