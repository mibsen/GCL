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

let getInputMap path = 

    printfn "%s" path
    printfn "Reading file content"


    let input = getInput path

    try
        let lexbuf = LexBuffer<char>.FromString input
        
        try 
           let res = InputParser.start InputLexer.tokenize lexbuf
           inputInterpret res Map.empty
           
         with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
     with e -> printfn "ERROR: %s" e.Message
              