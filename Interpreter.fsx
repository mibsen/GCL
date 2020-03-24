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

#load "IP.fs"
open IP

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

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
 
       try
        let final = createNode "qE" [] 
        final.Final <- true
        let (compiled,_) = PG.buildC res final 0 deterministic
   

       // We  now have the compiled Program graph, which need to be run with the inserted values
        let memory = Map.empty.Add("i",0).Add("j",0).Add("n",3).Add("A[0]",10).Add("A[1]",6).Add("A[2]",0)

        try
            run compiled memory
            printfn "-------"
        with e -> printfn "%s" e.Message

       with e -> printfn "%s" e.StackTrace
     with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
 with e -> printfn "ERROR: %s" e.Message
