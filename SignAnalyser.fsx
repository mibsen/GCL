#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
open System.IO

#load "SignAnalyser/SignInput.fs"
open SignInput

#load "SignAnalyser/SignInputParser.fs"
open SignInputParser

#load "SignAnalyser/SignInputLexer.fs"
open SignInputLexer

#load "SignAnalyser/SignInputInterpret.fs"
open SignInputInterpret

#load "LexerAndParser/GCLAST.fs"
open GCLAST

#load "LexerAndParser/GCLParser.fs"
open GCLParser

#load "LexerAndParser/GCLLexer.fs"
open GCLLexer

#load "Compiler/PG.fs"
open PG

#load "SignAnalyser/SignAnalysis.fs"
open SignAnalysis

let getInput path=
    try 
        File.ReadAllText path
    with e -> 
        failwith ("could not read file: " + path)
        null

let getPath = 
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        args.[2]
     
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   

let getInitSigns path: Map<string, int> * Map<string, Set<int>> = 

    let input = getInput path

    try
        let lexbuf = LexBuffer<char>.FromString input
        
        try 
           let res = SignInputParser.start SignInputLexer.tokenize lexbuf
           inputInterpret res (Map.empty, Map.empty)
           
         with e -> printfn "Parse error at : Line %i, %i" (lexbuf.EndPos.pos_lnum + 1) (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol)
                   (Map.empty, Map.empty)
     with e -> printfn "ERROR: %s" e.Message
               (Map.empty, Map.empty)


let getDeterministic =
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        Boolean.Parse(args.[4])
    with e -> 
        false

let getInitSignVars =
    try 
        let args: string array = System.Environment.GetCommandLineArgs()
        args.[3]
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   

let path = getPath
let deterministic = getDeterministic

printfn "Deterministic: %b" deterministic
printfn "%s" path
printfn "Reading file content"

let input = getInput path

let initAbsMem = Map.empty.Add("qS", Set.empty.Add(getInitSigns getInitSignVars))

try
    let lexbuf = LexBuffer<char>.FromString input
    
    try 
       let res = GCLParser.start GCLLexer.tokenize lexbuf
 
       let final = Node("qE")                                
       let start = Node("qS")
       let (edgeList,_) = buildC start res final 1 deterministic
                                 
       let abstractMemory = Analyse [start] edgeList initAbsMem

       //Map.iter (fun q nam -> printfn "%s: %s" q (nam.ToString())) abstractMemory

       //Map.iter (fun q nam -> printfn "%s: \n%s" q (Set.foldBack (fun (varSigns, arrSigns) a -> (sprintf "%s %s \n" (Map.foldBack (fun var x a -> (sprintf "(%s, %i)" var x)+a) varSigns "") (Map.foldBack (fun var x a -> (sprintf "(%s, %s)" var (x.ToString()))+a) arrSigns ""))+a) nam "")) abstractMemory

       printfn "%s" (PrintAbstractMemory abstractMemory)

     with e -> printfn "%s \n %s" e.Message e.StackTrace
 with e -> printfn "ERROR: %s" e.Message