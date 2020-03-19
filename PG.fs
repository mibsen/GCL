module PG

//Data structure for the program graph
type Node =
    {mutable Name : string;
    mutable Edges : Edge list;
    mutable Printed : bool;
    }
and Edge = // needs one for each unique edge logic
    |  AssignE of (x*a*Node)
    |  ArrayAssignE of (x * a * a * Node)
    |  SkipE of (Node)
    |  ChoiceE of (b * Node)
    
// Printing Edge Labels for the arithmetics and booleans
let rec getBString b =
    match b with
    | Bool (b) -> String.collect (fun c -> (string (System.Char.ToLower c))) (string b)
    | SAnd (b1 , b2) -> sprintf "(%s)&&(%s)" (getBString b1) (getBString b2) 
    | SOr (b1 , b2) -> sprintf "(%s)||(%s)" (getBString b1) (getBString b2) 
    | And (b1 , b2) -> sprintf "(%s)&(%s)" (getBString b1) (getBString b2) 
    | Or (b1 , b2) -> sprintf "(%s)|(%s)" (getBString b1) (getBString b2) 
    | Not (b) ->  sprintf "!(%s)" (getBString b)
    | Gt (a1, a2) -> sprintf "%s>%s" (getAString a1) (getAString a2) 
    | Lt (a1, a2) -> sprintf "%s<%s" (getAString a1) (getAString a2) 
    | Le (a1, a2) -> sprintf "%s<=%s" (getAString a1) (getAString a2) 
    | Ge (a1, a2) -> sprintf "%s>=%s" (getAString a1) (getAString a2) 
    | Eq (a1, a2) -> sprintf "%s=%s" (getAString a1) (getAString a2)
    | NotEq (a1, a2) -> sprintf "%s!=%s" (getAString a1) (getAString a2)
and getAString a =
    match a with
    | N n-> sprintf "%i" n
    | X x-> sprintf "%s" x
    | ArrayAccess (x , a) ->  sprintf "%s[%s]" x (getAString a)
    | Plus (a1 , a2) -> sprintf "%s+%s" (getAString a1) (getAString a2)
    | Minus (a1, a2) -> sprintf "%s-%s" (getAString a1) (getAString a2)
    | Multiply (a1, a2) -> sprintf "%s*%s" (getAString a1) (getAString a2)
    | Divide (a1, a2) -> sprintf "%s/%s" (getAString a1) (getAString a2)
    | Pow (a1, a2) -> sprintf "%s^%s" (getAString a1) (getAString a2)
    | UMinus (a) -> sprintf "-%s" (getAString a)
let createNode name edges = 
                            {Name = name; Edges = edges; Printed = false}
let getNodeName number = if number = 0 then "qS" else sprintf "q%i" number
let createNodeN number edges =  createNode (getNodeName number) edges

//Construction of command edges. Special logic is need for sequential compositioning
//and If and Do statements since they themselves contain commands
let rec buildC c final n deterministic = 
    match c with
    | Assign (x , a) ->  (createNodeN n [AssignE(x,a,final)]) , n+1 
    | ArrayAssign (x,a1,a2) -> (createNodeN n [ArrayAssignE(x,a1,a2,final)]) , n+1
    | Skip -> (createNodeN n [SkipE (final)]), n+1
    | Sequential (C1 , C2) ->
                               let nnn = createNodeN n [] 
                               let (no,n2) = buildC C1 nnn (n) deterministic
                               let (nno, n3) = buildC C2 final (n2) deterministic                                                            
                               nnn.Name <- nno.Name 
                               nnn.Edges <- nno.Edges 
                               no, n3
    | If (gc) -> let (edges,n2, _) = buildGC gc final (n+1) (Bool(false)) deterministic  
                 (createNodeN n edges), n2
    | Do (gc) -> let node =  (createNodeN n []) 
                 let boolconds = FindBoolConditions gc
                 let (edges,n2, _) = buildGC gc node (n+1) (Bool(false)) deterministic 
                 node.Edges <- List.append edges [ChoiceE(Not(boolconds),final)]
                 node, n2

//The negation of this funtions output corresponds to the "done" function in the book Formal Methods
and FindBoolConditions gc  = 
    match gc with
    | Choice (b , _) -> b
    | Conditional (gc1 , gc2) -> Or((FindBoolConditions gc2), (FindBoolConditions gc1))

//Construction of guarded command edges. Different logic is needed for constructing deterministic
//and non-determinictic program graphs
and buildGC gc final n prev deterministic = 
    if deterministic then
        match gc with
        | Choice (b , C) -> let (node,n2) = buildC C final (n) deterministic
                            [ChoiceE (And(b, Not(prev)), node)] , n2, Or(b, prev)                 
        | Conditional (gc1 , gc2) -> let (Edges,n1, prev2) = buildGC gc1 final n prev deterministic
                                     let (Edges2,n2, prev3) = buildGC gc2 final n1 prev2 deterministic
                                     (List.append Edges Edges2 ), n2, prev3
    else
        match gc with
        | Choice (b , C) -> let (node,n2) = buildC C final (n) deterministic
                            [ChoiceE (b, node)] , n2, prev
        | Conditional (gc1 , gc2) -> let (Edges,n1, _) = buildGC gc1 final n (Bool(false)) deterministic
                                     let (Edges2,n2, _) = buildGC gc2 final n1 (Bool(false)) deterministic
                                     (List.append Edges Edges2 ), n2, prev

//Functions for printing the program graph in the graphviz format                                                                   
let rec printNode com printed = if not (List.contains com.Name printed )
                                then
                                    let p = com.Name::printed
                                    printEdges com.Edges com.Name p
                                else
                                    printed
and printEdges edges name printed = 
    match edges with
    | [] -> printed
    | edge::rest -> let p = printEdges rest name printed
                    let p2 = printEdge edge name p
                    List.append p p2

and printEdge edge name printed = 
    match edge with
    |  AssignE (x,a,node) ->  printfn "%s -> %s [label = \"%s:=%s\"];" name node.Name x (getAString a)
                              printNode node printed

    |  ArrayAssignE (x,a1,a2,node) ->  printfn "%s -> %s [label = \"%s[%s]:=%s\"];" name node.Name x (getAString a1) (getAString a2)
                                       printNode node printed
    |  SkipE (node) ->   printfn "Skip" 
                         printNode node printed

    | ChoiceE (b, node) -> printfn "%s -> %s [label = \"%s\"];" name node.Name (getBString b)
                           printNode node printed 

and printGraph com =
    printfn "digraph program_graph {rankdir=LR;"
    printfn "node [shape = circle]; qS;"
    printfn "node [shape = doublecircle]; qE;"
    printfn "node [shape = circle]"
    let _ = printNode com []
    printfn "}"