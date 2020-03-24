module PG

type Node =
    | Node of (string)
and Edge =
    | Edge of (Node*Logic*Node) 
and Logic =
    | AssignE of (x*a)
    | ArrayAssignE of (x * a * a)
    | SkipE
    | BoolE of (b)
    
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

//Construction of command edges. Special logic is need for sequential compositioning
//and If and Do statements since they themselves contain commands
let rec gcDone gc  = 
    match gc with
        | Choice (b, _) -> b
        | Conditional (gc1 , gc2) -> Or((gcDone gc2), (gcDone gc1))

let rec buildC (start: Node) c (final: Node) (n: int) (deterministic: bool): Edge list * int = 
    match c with
        | Assign(x, a) ->  [Edge(start, AssignE(x,a), final)], n
        | ArrayAssign(x, a1, a2) -> [Edge(start, ArrayAssignE(x,a1,a2), final)], n
        | Skip -> [Edge(start, SkipE, final)], n
        | Sequential(C1, C2) ->  let fresh = Node ("q"+string(n))
                                 let (edges1, n2) = buildC start C1 fresh (n+1) deterministic
                                 let (edges2, n3) = buildC fresh C2 final n2 deterministic
                                 edges1 @ edges2, n3
        | If(gc) -> let (edges,n2,_) = buildGC start gc final n (Bool(false)) deterministic
                    edges, n2
        | Do(gc) -> let (edges,n2,_) = buildGC start gc start n (Bool(false)) deterministic
                    Edge(start, BoolE(Not(gcDone gc)), final)::edges, n2

and buildGC (start: Node) gc (final: Node) (n: int) (prev: b) (deterministic: bool): Edge list * int * b = 
    if deterministic then
        match gc with
            | Choice(b , C) -> let fresh = Node("q"+string(n))
                               let (edges,n2) = buildC fresh C final (n+1) deterministic
                               [Edge(start, BoolE(And(b, Not(prev))), fresh)] @ edges , n2, Or(b, prev)  
            | Conditional (gc1 , gc2) -> let (Edges, n1, prev2) = buildGC start gc1 final n prev deterministic
                                         let (Edges2, n2, prev3) = buildGC start gc2 final n1 prev2 deterministic
                                         Edges @ Edges2, n2, prev3
    else
        match gc with
            | Choice (b , C) -> let fresh = Node("q"+string(n))
                                let (edges,n2) = buildC fresh C final (n+1) deterministic
                                (Edge(start, BoolE(Not(b)), fresh))::edges , n2, prev
            | Conditional (gc1 , gc2) -> let (Edges, n1, _) = buildGC start gc1 final n (Bool(false)) deterministic
                                         let (Edges2, n2, _) = buildGC start gc2 final n1 (Bool(false)) deterministic
                                         Edges @ Edges2, n2, prev

let rec printEdges es =
    match es with
        |[] -> ""
        |Edge(n1, e2, n2)::res ->  (printLogic e2 (printNode n1) (printNode n2))+"\n"+ (printEdges res)
and printLogic e n1 n2 =
    match e with
        |AssignE(x, a) -> sprintf "%s -> %s [label = \"%s:=%s\"];" n1 n2 x (getAString a)
        |ArrayAssignE(x, a1, a2) -> sprintf "%s -> %s [label = \"%s[%s]:=%s\"];" n1 n2 x (getAString a1) (getAString a2)
        |SkipE -> "Skip" 
        |BoolE(s) -> sprintf "%s -> %s [label = \"%s\"];"  n1 n2 (getBString s)
and printNode n =
    match n with
        |Node(s) -> s
and printGraph com =
    "digraph program_graph {rankdir=LR;
node [shape = circle]; qS;
node [shape = doublecircle]; qE;
node [shape = circle]\n" +
    printEdges com +
    "}"