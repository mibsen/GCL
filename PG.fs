module PG

type Node =
    | Final of (string)
    | Node of (string * Edge list) 
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

let rec buildC c final n = 
    match c with
    | Assign (x , a) ->  [AssignE(x,a,final)] , n 
    | ArrayAssign (x,a1,a2) -> [ArrayAssignE(x,a1,a2,final)] , n
    | Skip -> [SkipE (final)], n
    | Sequential (C1 , C2) ->  let (edges,n2) = buildC C2 final (n + 1)
                               let q = Node ("q"+string(n)  , edges )
                               buildC C1 q n2
    | If (gc) ->  buildGC gc final n
//    | Do (gc) -> sprintf "Do(%s)" (printGC gc)
and buildGC gc final n = 
    match gc with
    | Choice (b , C) -> let (edges,n2) = buildC C final (n+1)
                        [ChoiseE (b, Node ("q"+ string(n), edges))] , n2
    | Conditional (gc1 , gc2) -> let (Edges,n1) = buildGC gc1 final n
                                 let (Edges2,n2) = buildGC gc2 final n1
                                 (List.append Edges Edges2 ), n2
                              
let rec printNode com = 
    match com with
    | Node (name,edges) -> List.forall (fun e -> printEdge e name ) edges                        
    | _ ->  true
and getNodeName n =
    match n with
    | Node (name,_) -> name
    | Final (name) -> name
and printEdge edge name = 
    match edge with
    |  AssignE (x,a,node) ->  printfn "%s -> %s [label = \"%s=%s\"];" name (getNodeName node) x (getAString a)
                              printNode node
    |  ArrayAssignE (x,a1,a2,node) ->  printfn "%s -> %s [label = \"%s[%s]=%s\"];" name (getNodeName node) x (getAString a1) (getAString a2)
                                       printNode node
    |  SkipE (node) ->   printfn "Skip" 
                         printNode node
    | ChoiseE (b, node) -> printfn "%s -> %s [label = \"%s\"];" name (getNodeName node) (getBString b)
                           printNode node  
and printGraph com =
    printfn "digraph program_graph {rankdir=LR;"
    printfn "node [shape = circle]; q▷;"
    printfn "node [shape = doublecircle]; q◀;"
    printfn "node [shape = circle]"
    let _ = printNode com
    printfn "}"