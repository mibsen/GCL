#!/bin/sh

./packages/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ./GCLLexer.fsl --unicode
./packages/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ./GCLParser.fsp --module GCLParser
# fsi.exe ../GCL.fsx
