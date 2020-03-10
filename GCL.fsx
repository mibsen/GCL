#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
open System.IO


#load "GCLAST.fs"
open GCLAST

#load "GCLParser.fs"
open GCLParser

#load "GCLLexer.fs"
open GCLLexer


let getPath = 
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        args.[2]
     
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   

let path = getPath

printfn "%s" path
printfn "Reading file content"


let getInput path=
    try 
        File.ReadAllText path
    with e -> 
        failwith ("could not read file: " + path)
        null

let input = getInput path

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
       printfn "COMPILED - Pretty print"
       printfn "%s" (GCLAST.print res)
       
        
     with e -> printfn "Parse error at : Line %i, %i" lexbuf.EndPos.Line lexbuf.EndPos.Column
               
 with e -> printfn "ERROR: %s" e.Message
          