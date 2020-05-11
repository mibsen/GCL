#  A parser for GCL
We have implemented a parser for GCL Which accepts or rejects programs and builds AST for them. 

The solution is built using FsLexYacc.

## How to build
1. Compile the GCL lexer using fslex.exe
`./FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ./GCLLexer.fsl --unicode`

2. Compile the GCL parser using fsyacc.exe
`./FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ./GCLParser.fsp --module GCLParser`

3. Compile the input lexer using fslex.exe
`./packages/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ./InputLexer.fsl --unicode`

4. Compile the input parser using fsyacc.exe
`./packages/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ./InputParser.fsp --module InputParser`


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

## How to run the sign analyser
`fsi.exe SignAnalyser.fsx <Path to source file> <Path to file with initial abstract memory> <true or false to construct deterministic graph (false is default)>`

This will print the analysis assignment for the program with the given initial abstract memory. An example output would be:

       x    y

`qS:`

       +    +
   
`q1:`

       +    +
    
`q2:`

       -    +
    
       0    +
    
       +    +
    
`q3:`

       +    +
    
`q4:`

       +    +
    
`qE:`

       -    +
    
       0    +


The file containing the initial abstract memory should follow the same general syntax as the initialization of the abstract memory in http://www.formalmethods.dk/fm4fun/#sign-analysis, with the exception that a set of abstract memories is specified by placing a semicolon between the abstract memories. For example:

`x = +, y = +, A = {0};`

`x = +, y = +, A = {-}`

## How to run the security analyser
`fsi.exe SecurityAnalyser.fsx <Path to source file> <Path to file with lattice and classifications>`

This will print the security analysis for the program with the given lattice and classifications. An example output would be:

`Actual flows:`

`A->A, B->B, i->A, i->B, i->i, i->j, j->A, j->B, j->i, j->j, m->A, m->B, m->i, m->j, n->A, n->B, n->i, n->j,`

`Allowed flows:`

`A->A, A->B, A->i, A->j, A->m, A->n, B->B, i->A, i->B, i->i, i->j, i->m, i->n, j->B, j->j, j->n, m->A, m->B, m->i, m->j, m->m, m->n, n->B, n->n,`

`Violations:`

`j->A, j->i, n->A, n->i, n->j,`

`Result:`

`Not Secure`

The syntax of file containing the lattice and classifications differs from http://www.formalmethods.dk/fm4fun/#sign-analysis. A multi-level lattice is defined by:

`unclassified <  classified < secret < topSecret`

The classification has the same syntax. The classification and the lattice should be in the same file separated by a semicolon with the lattice first. The full input should then look like:

`unclassified <  classified < secret < topSecret ; i = unclassified, j = classified, m = unclassified, n = secret , A = unclassified, B = topSecret`

## How to run the model checker
`fsi.exe ModelChecker.fsx <Path to source file> <Path to file with initial variables> <true or false to construct deterministic graph (false is default)>`

This will print each stuck state and whether or not that state is a terminal state. An example output would be:

`status: stuck`

`Node: q4`

`x: 0`

`y: 0`

`z: 0`


`status: terminated`

`Node: qE`

`x: 0`

`y: 0`

`z: 0`

The file containing the initial variables should follow the same syntax as the initialization of variables in http://www.formalmethods.dk/fm4fun. For example:

`x = 0, y = 0, z = 0, A = [1,2,3,4]`


## Tests
Multiple test cases are placed in the `./test/cases` folder.


