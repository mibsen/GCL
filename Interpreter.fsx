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
      
let path = getPath
let deterministic = getDeterministic


printfn "Deterministic: %b" deterministic
printfn "%s" path
printfn "Reading file content"


let getInput path=
    try 
        File.ReadAllText path
    with e -> 
        failwith ("could not read file: " + path)
        null

let input = getInput path

let map =
   Map.empty. (* Creating an empty Map *)
      Add("x", 0).
      Add("y", 0).
      Add("i", 0).
      Add("n", 0).
      Add("A0", 1).
      Add("A1", 2).
      Add("A2", 3).
      Add("A3", 15);;

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
 
       try
        let final = Node("qE")                                
        let start = Node("qS")
        let (edgeList,_) = buildC start res final 1 deterministic
        let (finalnode, intmap) = Interpret.interpret start edgeList edgeList map
      
        if finalnode = "qE" then
            printfn "status: terminated"
        else 
            printfn "status: stuck"
        printfn "Node: %s" finalnode
        Map.iter (fun s i -> printfn "%s: %i" s i) intmap 
         

       with e -> printfn "%s" (string e)
     with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
 with e -> printfn "ERROR: %s" e.Message
