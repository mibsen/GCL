#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
open System.IO

#load "Util.fsx"
open Util

#load "LexerAndParser/GCLAST.fs"
open GCLAST

#load "LexerAndParser/GCLParser.fs"
open GCLParser

#load "LexerAndParser/GCLLexer.fs"
open GCLLexer

#load "Compiler/PG.fs"
open PG


let args: string array = System.Environment.GetCommandLineArgs()
let path = getPath args
let deterministic = getDeterministic args

printfn "Deterministic: %b" deterministic
printfn "%s" path
printfn "Reading file content"


let input = getInput path

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
 
       let final = Node("qE")                                
       let start = Node("qS")
       let (edgeList,_) = buildC start res final 1 deterministic
                                 
   
       printfn "COMPILED - Printing Graph"
       printfn "-------"
       printfn "%s" (printGraph edgeList)
       
     with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
 with e -> printfn "ERROR: %s" e.Message