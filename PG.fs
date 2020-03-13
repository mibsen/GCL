﻿module PG

type Node =
    {mutable Name : string;
    mutable Edges : Edge list;
    mutable Printed : bool;
    }
and Edge = // needs one for each unique edge logic
    |  AssignE of (x*a*Node)
    |  ArrayAssignE of (x * a * a * Node)
    |  SkipE of (Node)
    |  ChoiseE of (b * Node)
    
// Printing Edge Labels for the arithmetics and booleans
// TODO: this might need some brackets
let rec getBString b =
    match b with
    | Bool (b) -> string b
    | SAnd (b1 , b2) -> sprintf "%s && %s" (getBString b1) (getBString b2) 
    | SOr (b1 , b2) -> sprintf "%s || %s" (getBString b1) (getBString b2) 
    | And (b1 , b2) -> sprintf "%s & %s" (getBString b1) (getBString b2) 
    | Or (b1 , b2) -> sprintf "%s | %s" (getBString b1) (getBString b2) 
    | Not (b) ->  sprintf "!%s" (getBString b)
    | Gt (a1, a2) -> sprintf "%s > %s" (getAString a1) (getAString a2) 
    | Lt (a1, a2) -> sprintf "%s < %s" (getAString a1) (getAString a2) 
    | Le (a1, a2) -> sprintf "%s >= %s" (getAString a1) (getAString a2) 
    | Ge (a1, a2) -> sprintf "%s <= %s)" (getAString a1) (getAString a2) 
    | Eq (a1, a2) -> sprintf "%s = %s)" (getAString a1) (getAString a2)
    | NotEq (a1, a2) -> sprintf "%s != %s" (getAString a1) (getAString a2)
and getAString a =
    match a with
    | N n-> sprintf "%f" n
    | X x-> sprintf "%s" x
    | ArrayAccess (x , a) ->  sprintf "%s[%s]" x (getAString a)
    | Plus (a1 , a2) -> sprintf "%s + %s" (getAString a1) (getAString a2)
    | Minus (a1, a2) -> sprintf "%s - %s" (getAString a1) (getAString a2)
    | Multiply (a1, a2) -> sprintf "%s * %s" (getAString a1) (getAString a2)
    | Divide (a1, a2) -> sprintf "%s / %s" (getAString a1) (getAString a2)
    | Pow (a1, a2) -> sprintf "%s^%s" (getAString a1) (getAString a2)
    | UMinus (a) -> sprintf "-%s" (getAString a)
let createNode name edges = 
                            {Name = name; Edges = edges; Printed = false}
let getNodeName number = if number = 0 then "Start" else sprintf "q%i" number
let createNodeN number edges =  createNode (getNodeName number) edges

let rec buildC c final n = 
    match c with
    | Assign (x , a) ->  (createNodeN n [AssignE(x,a,final)]) , n+1 
    | ArrayAssign (x,a1,a2) -> (createNodeN n [ArrayAssignE(x,a1,a2,final)]) , n+1
    | Skip -> (createNodeN n [SkipE (final)]), n+1
    | Sequential (C1 , C2) ->
                               let (no,n2) = buildC C2 final (n + 1)
                               let (nno, n3) = buildC C1 no n2                                                           
                               nno.Name <- getNodeName n
                               nno, n3
    | If (gc) -> let (edges,n2) = buildGC gc final (n+1)  
                 (createNodeN n edges), n2
    | Do (gc) -> let node =  (createNodeN n []) 
                 let (edges,n2) = buildGC gc node (n+1) 
                 node.Edges <- List.append edges [ChoiseE(FindBoolConditions edges,final)]
                 node, n2

and FindBoolConditions edges  = 
    match edges with
    | ChoiseE(b,_) :: [] -> Not(b)
    | ChoiseE(b,_)::rest -> And(Not (b),FindBoolConditions rest)
    | _::rest -> FindBoolConditions rest

and buildGC gc final n = 
    match gc with
    | Choice (b , C) -> let (node,n2) = buildC C final (n)
                        [ChoiseE (b, node)] , n2
    | Conditional (gc1 , gc2) -> let (Edges,n1) = buildGC gc1 final n
                                 let (Edges2,n2) = buildGC gc2 final n1
                                 (List.append Edges Edges2 ), n2
                              
let rec printNode com printed = if not (List.contains com.Name printed )
                                then
                                    let p = com.Name::printed
                                    List.iter (fun e -> printEdge e com.Name p ) com.Edges                              
and printEdge edge name printed = 
    match edge with
    |  AssignE (x,a,node) ->  printfn "%s -> %s [label = \"%s=%s\"];" name node.Name x (getAString a)
                              printNode node printed

    |  ArrayAssignE (x,a1,a2,node) ->  printfn "%s -> %s [label = \"%s[%s]=%s\"];" name node.Name x (getAString a1) (getAString a2)
                                       printNode node printed
    |  SkipE (node) ->   printfn "Skip" 
                         printNode node printed

    | ChoiseE (b, node) -> printfn "%s -> %s [label = \"%s\"];" name node.Name (getBString b)
                           printNode node printed 

and printGraph com =
    printfn "digraph program_graph {rankdir=LR;"
    printfn "node [shape = circle]; q▷;"
    printfn "node [shape = doublecircle]; q◀;"
    printfn "node [shape = circle]"
    let _ = printNode com []
    printfn "}"