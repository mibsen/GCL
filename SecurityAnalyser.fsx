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

#load "SecurityAnalyser/SecurityAnalysis.fs"
open SecurityAnalysis

#load "SecurityAnalyser/SecurityInput.fs"
open SecurityInput

#load "SecurityAnalyser/SecurityInputParser.fs"
open SecurityInputParser

#load "SecurityAnalyser/SecurityInputLexer.fs"
open SecurityInputLexer

let getLatticeAndClassification path = 

    let input = getInput path

    try
        let lexbuf = LexBuffer<char>.FromString input
        
        try 
           let res = SecurityInputParser.start SecurityInputLexer.tokenize lexbuf
           
           inputInterpret res

         with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
                   Set.empty, Map.empty
     with e -> printfn "ERROR: %s" e.StackTrace
               Set.empty, Map.empty

let args: string array = System.Environment.GetCommandLineArgs()
let path = getPath args

let (lattice, securityClassification) = getLatticeAndClassification (getInitVals args)

printfn "%s" path
printfn "Reading file content"

let input = getInput path

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
 
       let flows = buildC res Set.empty

       printfn "Actual flows"
       Set.iter (fun (s,i) -> printf "%s->%s, " s i) flows 

       let allowedFlows = computeAllowedFlows securityClassification lattice

       printfn "\n\nAllowed flows"
       Set.iter (fun (s,i) -> printf "%s->%s, " s i) allowedFlows 

       let violations = Set.foldBack (fun flow (a: Set<string*string>) -> if allowedFlows.Contains flow then a else a.Add(flow)) flows Set.empty

       printfn "\n\nViolations"
       Set.iter (fun (s,i) -> printf "%s->%s, " s i) violations 

       printfn "\n\nResult"
       if not violations.IsEmpty then printfn "Not Secure" else printfn "Secure"

     with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
 with e -> printfn "ERROR: %s" e.Message