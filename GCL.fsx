#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "GCLAST.fs"
open GCLAST

#load "GCLParser.fs"
open GCLParser

#load "GCLLexer.fs"
open GCLLexer

let input = Console.ReadLine()
printfn "%s" input
try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
       printfn "COMPILED - Pretty print"
       printfn "%s" (GCLAST.print res)
       
        
     with e -> printfn "Parse: %s" e.Message
               
 with e -> printfn "ERROR: %s" e.Message
          