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

#load "Interpreter/Input.fs"
open Input

#load "Interpreter/InputParser.fs"
open InputParser

#load "Interpreter/InputLexer.fs"
open InputLexer

#load "Interpreter/InputInterpret.fs"
open InputInterpret

#load "Compiler/PG.fs"
open PG

#load "Interpreter/Interpret.fs"
open Interpret

let getInputMap path: Map<string, int> = 

    let input = getInput path

    try
        let lexbuf = LexBuffer<char>.FromString input
        
        try 
           let res = InputParser.start InputLexer.tokenize lexbuf
           inputInterpret res Map.empty
           
         with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
                   Map.empty
     with e -> printfn "ERROR: %s" e.Message
               Map.empty

let args: string array = System.Environment.GetCommandLineArgs()
let path = getPath args
let deterministic = getDeterministic args
let inputMap = getInputMap (getInitVals args)

printfn "Deterministic: %b" deterministic
printfn "%s" path
printfn "Reading file content"

let input = getInput path

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
 
       try
        let final = Node("qE")                                
        let start = Node("qS")
        let (edgeList,_) = buildC start res final 1 deterministic
        let (finalnode, intmap) = Interpret.interpret start edgeList edgeList inputMap
      
        if finalnode = "qE" then
            printfn "status: terminated"
        else 
            printfn "status: stuck"
        printfn "Node: %s" finalnode
        Map.iter (fun s i -> printfn "%s: %i" s i) intmap 

       with e -> printfn "%s" (string e)
     with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
 with e -> printfn "ERROR: %s" e.Message
