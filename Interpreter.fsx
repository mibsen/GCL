#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
open System.IO

#load "Input.fs"
open Input

#load "InputParser.fs"
open InputParser

#load "InputLexer.fs"
open InputLexer

#load "InputInterpret.fs"
open InputInterpret

let getInput path=
    try 
        File.ReadAllText path
    with e -> 
        failwith ("could not read file: " + path)
        null

let getInputMap path: Map<string, int> = 

    printfn "%s" path
    printfn "Reading file content"


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

#load "GCLAST.fs"
open GCLAST

#load "GCLParser.fs"
open GCLParser

#load "GCLLexer.fs"
open GCLLexer

#load "PG.fs"
open PG

#load "Interpret.fs"
open Interpret

let getPath = 
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        args.[2]
     
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   

let getDeterministic =
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        Boolean.Parse(args.[3])
    with e -> 
        false

let getInitVars =
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        args.[4]
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   

let path = getPath
let deterministic = getDeterministic
let inputMap = getInputMap getInitVars
Map.iter (fun s i -> printfn "%s: %i" s i) inputMap

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
        printfn "%s" (printGraph edgeList) 
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