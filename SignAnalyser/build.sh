#!/bin/sh

../packages/FsLexYacc.10.0.0/build/fslex/net46/fslex.exe ./SignInputLexer.fsl --unicode
../packages/FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe ./SignInputParser.fsp --module SignInputParser
# fsi.exe ../GCL.fsx
