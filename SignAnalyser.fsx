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

#load "SignAnalyser/SignInput.fs"
open SignInput

#load "SignAnalyser/SignInputParser.fs"
open SignInputParser

#load "SignAnalyser/SignInputLexer.fs"
open SignInputLexer

#load "SignAnalyser/SignInputInterpret.fs"
open SignInputInterpret

#load "Compiler/PG.fs"
open PG

#load "SignAnalyser/SignAnalysis.fs"
open SignAnalysis


let getInitSignsList path: NodeAbsMem = 

    let input = getInput path

    try
        let lexbuf = LexBuffer<char>.FromString input
        
        try 
           let res = SignInputParser.start SignInputLexer.tokenize lexbuf

           let rec toSet r = 
               match r with
                   | Line(l) -> Set.empty.Add(inputInterpret l (Map.empty, Map.empty))
                   | Lines(l,res) -> (toSet res).Add(inputInterpret l (Map.empty, Map.empty))

           toSet res
           
         with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
                   Set.empty.Add(Map.empty, Map.empty)
     with e -> printfn "ERROR: %s" e.StackTrace
               Set.empty.Add(Map.empty, Map.empty)


let args: string array = System.Environment.GetCommandLineArgs()
let path = getPath args
let deterministic = getDeterministic args

printfn "Deterministic: %b" deterministic
printfn "%s" path
printfn "Reading file content"

let input = getInput path
let initSignsSet = getInitSignsList (getInitVals args)
let initAbsMem = Map.empty.Add("qS", initSignsSet)

try
    let lexbuf = LexBuffer<char>.FromString input

    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf

       
       let final = Node("qE")                                
       let start = Node("qS")
       let (edgeList,_) = buildC start res final 1 deterministic
                             
       let abstractMemory = Analyse [start] edgeList initAbsMem

       printfn "%s" (PrintAbstractMemory abstractMemory)

    with e -> printfn "%s \n %s" e.Message e.StackTrace
with e -> printfn "ERROR: %s" e.Message