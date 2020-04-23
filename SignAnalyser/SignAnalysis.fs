module SignAnalysis

//Data structure for the abstract memories of a node. Variables are a string * int map and arrays are a string * Set<int> map.
//Sets are used to prevent duplicates
type NodeAbsMem = Set<Map<string, int> * Map<string, Set<int>>>

type a = 
    | N of int
    | X of x
    | ArrayAccess of (x * a)
    | Plus of (a * a)
    | Minus of (a * a)
    | Multiply of (a * a)
    | Divide of (a * a)
    | Pow of (a * a)
    | UMinus of (a)

and x = string

and b = 
    | Bool of bool
    | SAnd of (b * b)
    | SOr of (b * b)
    | And of (b * b)
    | Or of (b * b)
    | Not of (b)
    | Gt of (a * a)
    | Lt of (a * a)
    | Le of (a * a)
    | Ge of (a * a)
    | Eq of (a * a)
    | NotEq of (a * a)

type Node =
    | Node of (string)
and Edge =
    | Edge of (Node*Logic*Node) 
and Logic =
    | AssignE of (x*a)
    | ArrayAssignE of (x * a * a)
    | SkipE
    | BoolE of (b)

//Results of operations on signs
let plusMap = Map.empty.Add(-1, Map.empty.Add(-1, [-1])    .Add(0, [-1]).Add(1, [-1;0;1]))
                       .Add( 0, Map.empty.Add(-1, [-1])    .Add(0, [0]) .Add(1, [1]))
                       .Add( 1, Map.empty.Add(-1, [-1;0;1]).Add(0, [1]) .Add(1, [1]))
let minusMap = Map.empty.Add(-1, Map.empty.Add(-1, [-1;0;1]).Add(0, [-1]).Add(1, [-1]))
                        .Add( 0, Map.empty.Add(-1, [1])     .Add(0, [0]) .Add(1, [-1]))
                        .Add( 1, Map.empty.Add(-1, [1])     .Add(0, [1]) .Add(1, [-1;0;1]))
let multMap = Map.empty.Add(-1, Map.empty.Add(-1, [1]) .Add(0, [0]).Add(1, [-1]))
                       .Add( 0, Map.empty.Add(-1, [0]) .Add(0, [0]).Add(1, [0]))
                       .Add( 1, Map.empty.Add(-1, [-1]).Add(0, [0]).Add(1, [1]))       
let divMap = Map.empty.Add(-1, Map.empty.Add(-1, [1]) .Add(0, []).Add(1, [-1]))
                      .Add( 0, Map.empty.Add(-1, [0]) .Add(0, []).Add(1, [0]))
                      .Add( 1, Map.empty.Add(-1, [-1]).Add(0, []).Add(1, [1]))                                      
let powMap = Map.empty.Add(-1, Map.empty.Add(-1, []) .Add(0, [1]).Add(1, [-1]))
                      .Add( 0, Map.empty.Add(-1, []) .Add(0, []) .Add(1, [0]))
                      .Add( 1, Map.empty.Add(-1, [1]).Add(0, [1]).Add(1, [1]))    
let geMap = Map.empty.Add(-1, Map.empty.Add(-1, [true; false]).Add(0, [false]).Add(1, [false]))
                     .Add( 0, Map.empty.Add(-1, [true])       .Add(0, [true]) .Add(1, [false]))
                     .Add( 1, Map.empty.Add(-1, [true])       .Add(0, [true]) .Add(1, [true;false]))   
let leMap = Map.empty.Add(-1, Map.empty.Add(-1, [true; false]).Add(0, [true]) .Add(1, [true]))
                     .Add( 0, Map.empty.Add(-1, [false])      .Add(0, [true]) .Add(1, [true]))
                     .Add( 1, Map.empty.Add(-1, [false])      .Add(0, [false]).Add(1, [true;false]))                      
let eqMap = Map.empty.Add(-1, Map.empty.Add(-1, [true; false]).Add(0, [false]).Add(1, [false]))
                     .Add( 0, Map.empty.Add(-1, [false])      .Add(0, [true]) .Add(1, [false]))
                     .Add( 1, Map.empty.Add(-1, [false])      .Add(0, [false]).Add(1, [true;false]))                        


//Corresponds to the outer loop of the worklist algorithm on page 60
let rec Analyse (workList: Node list) (edgeList: Edge list) (absMem: Map<string, NodeAbsMem>) = 
    match workList with
        |[] -> absMem
        |node::res -> let (absMem2, workList2) = AnalyseNode node edgeList absMem []
                      Analyse (res @ workList2) edgeList absMem2

//Inner loop of the worklist algorithm        
and AnalyseNode (Node(s)) (edgeList: Edge list) (currentMem: Map<string, NodeAbsMem>) (workList: Node list) = 
    match edgeList with
        |[] -> currentMem, workList
        |Edge(Node(s1), logic, Node(s2))::xs when s = s1 ->  let computedNodeMem = LogicAnalysis (Set.toList (currentMem.Item s1)) logic Set.empty                                                      
                                                             if not (currentMem.ContainsKey s2) then //Node has not been visited yet
                                                                 AnalyseNode (Node(s1)) xs (currentMem.Add(s2, computedNodeMem)) (Node(s2)::workList)
                                                             elif not (Set.isSubset computedNodeMem (currentMem.Item s2)) then //Does the computed abstract memories contain an unseen memory for this node?
                                                                 let newNodeMem = Set.union (currentMem.Item s2) computedNodeMem
                                                                 AnalyseNode (Node(s1)) xs (currentMem.Add(s2, newNodeMem)) (Node(s2)::workList)
                                                             else
                                                                 AnalyseNode (Node(s1)) xs currentMem workList
        |_::xs -> AnalyseNode (Node(s)) xs currentMem workList

//Each abstract memory the start node has are evaluated on the logic
and LogicAnalysis (nodeAbsMem: (Map<string, int> * Map<string, Set<int>>) list) (logic: Logic) (computedNodeMem: NodeAbsMem): NodeAbsMem =
    match nodeAbsMem with
        |[] -> computedNodeMem
        |absMem::xs ->  let absMems, b = SignAnalysis absMem logic
                        if b then //Only add result to target node's abstract memory if b is true. Either a bool edge is evaluated to false or a negative array index is accessed
                            LogicAnalysis xs logic (Set.union computedNodeMem absMems)
                        else 
                            LogicAnalysis xs logic computedNodeMem

//The edge logic is applied to the abstract memory. Second return parameter is true if the edgeLogic could be applied succesfully
and SignAnalysis ((varSigns, arrSigns):Map<string, int> * Map<string, Set<int>>) (edgeLogic: Logic): Set<Map<string, int> * Map<string, Set<int>>> * bool =
    match edgeLogic with
        | AssignE(x, a) -> let signs = (ArithAnalysis varSigns arrSigns a)
                           //Create a new abstract memory for each sign that is returned
                           Set.foldBack (fun sign absMem -> absMem.Add(varSigns.Add(x, sign), arrSigns)) signs Set.empty, true
        | ArrayAssignE(x, a1, a2) ->let accesSigns = (ArithAnalysis varSigns arrSigns a1)
                                    if not (accesSigns.Contains(0) || accesSigns.Contains(1)) then //Can't access array if index can only be negative
                                        Set.empty.Add(varSigns, arrSigns), false
                                    else
                                        let signs = (ArithAnalysis varSigns arrSigns a2)
                                        
                                        //Each sign is added to the abstract memory without removing any signs
                                        let set1 = Set.foldBack (fun sign (absMem : NodeAbsMem) -> absMem.Add(varSigns, arrSigns.Add(x, (arrSigns.Item x).Add(sign)))) signs Set.empty
                                        
                                        //Signs are removed from the abstract memory, corresponding to the case where the last variable of a sign in an array is replaced with another sign
                                        let reduceSigns = Set.foldBack (fun sign a -> (Set.difference (arrSigns.Item x) (Set.empty.Add(sign)))::a) (arrSigns.Item x) []

                                        //New signs are added to the reduced abstract memories
                                        let set2 = List.foldBack (fun (f : Set<int>) (b : NodeAbsMem) -> Set.union b (Set.foldBack (fun sign absMem -> absMem.Add(varSigns, arrSigns.Add(x, (f.Add(sign))))) signs Set.empty)) reduceSigns Set.empty
                                        
                                        (Set.union set1 set2), true
        | SkipE -> Set.empty.Add(varSigns, arrSigns), true
        | BoolE(b) -> let result = BoolAnalysis varSigns arrSigns b
                      if result.Contains true then
                        Set.empty.Add(varSigns, arrSigns), true
                      else 
                        Set.empty.Add(varSigns, arrSigns), false

//Corresponds to the analysis function A on page 52 in Formal Methods
and ArithAnalysis (varSigns :Map<string, int>) (arrSigns:Map<string, Set<int>>) (a : a): Set<int> =
    match a with 
        | N(n) -> Set.empty.Add(sign n)
        | X(x) -> Set.empty.Add(varSigns.Item x)
        | ArrayAccess(x,_) -> arrSigns.Item x
        | Plus(a1, a2) -> SignOperation plusMap a1 a2 varSigns arrSigns
        | Minus(a1, a2) -> SignOperation minusMap a1 a2 varSigns arrSigns
        | Multiply(a1, a2) -> SignOperation multMap a1 a2 varSigns arrSigns
        | Divide(a1, a2) -> SignOperation divMap a1 a2 varSigns arrSigns
        | Pow(a1, a2) -> SignOperation powMap a1 a2 varSigns arrSigns
        | UMinus(a) -> Set.map (~-) (ArithAnalysis varSigns arrSigns a)    

//Method for getting the result of a sign operation
and SignOperation (signMap : Map<int, Map<int, int list>>) (a1: a) (a2: a) (varSigns :Map<string, int>) (arrSigns:Map<string, Set<int>>) : Set<int> =
    let a1signs = ArithAnalysis varSigns arrSigns a1
    let a2signs = ArithAnalysis varSigns arrSigns a2
    let a1maps = Set.foldBack (fun sign maplist -> (signMap.Item sign)::maplist) a1signs []
    Set.foldBack (fun sign signs -> Set.union (List.foldBack (fun (f:Map<int, int list>) b -> Set.union (Set.ofList (f.Item sign)) b) a1maps Set.empty) signs) a2signs Set.empty

//Corresponds to the analysis function B on page 53 in Formal Methods
and BoolAnalysis (varSigns :Map<string, int>) (arrSigns:Map<string, Set<int>>) (b : b) : Set<bool> =
    match b with
        | Bool (b) -> Set.empty.Add(b)
        | SAnd (b1 , b2) | And(b1, b2) -> BoolOperation (&&) b1 b2 varSigns arrSigns
        | SOr (b1 , b2) | Or(b1, b2) -> BoolOperation (||) b1 b2 varSigns arrSigns
        | Not (b) ->  Set.map (not) (BoolAnalysis varSigns arrSigns b)
        | Gt (a1, a2) -> Set.map (not) (SignComparison leMap a1 a2 varSigns arrSigns)
        | Lt (a1, a2) -> Set.map (not) (SignComparison geMap a1 a2 varSigns arrSigns)
        | Le (a1, a2) -> SignComparison leMap a1 a2 varSigns arrSigns
        | Ge (a1, a2) -> SignComparison geMap a1 a2 varSigns arrSigns
        | Eq (a1, a2) -> SignComparison eqMap a1 a2 varSigns arrSigns
        | NotEq (a1, a2) -> Set.map (not) (SignComparison eqMap a1 a2 varSigns arrSigns)

//Method for getting the result of a boolean operation
and BoolOperation op (b1 : b) (b2 : b) (varSigns :Map<string, int>) (arrSigns:Map<string, Set<int>>) : Set<bool> =
    let b1signs = BoolAnalysis varSigns arrSigns b1
    let b2signs = BoolAnalysis varSigns arrSigns b2
    //Boolean operations can be evaluated as normal
    Set.foldBack (fun bool1 bools -> Set.union (Set.foldBack (fun bool2 boolset -> Set.union (Set.empty.Add(op bool1 bool2)) boolset) b2signs Set.empty) bools) b1signs Set.empty

//Method for getting the result of a sign comparison
and SignComparison (operationMap : Map<int, Map<int, bool list>>) (a1: a) (a2: a) (varSigns :Map<string, int>) (arrSigns:Map<string, Set<int>>) : Set<bool> =
    let a1signs = ArithAnalysis varSigns arrSigns a1
    let a2signs = ArithAnalysis varSigns arrSigns a2
    let a1maps = Set.foldBack (fun sign maplist -> (operationMap.Item sign)::maplist) a1signs []
    Set.foldBack (fun sign signs -> Set.union (List.foldBack (fun (f:Map<int, bool list>) b -> Set.union (Set.ofList (f.Item sign)) b) a1maps Set.empty) signs) a2signs Set.empty


//Below is code used to print the final result of the sign analysis
let ConvertIntSignToSign i =
    match i with
        | -1 -> "-"
        | 0 -> "0"
        | _ -> "+"
   
let rec PrintSet l a =
    match l with
        |[] -> a
        |[x] -> a + (ConvertIntSignToSign x) + "}"
        |x::xs -> PrintSet xs (a+(sprintf "%s, " (ConvertIntSignToSign x)))

let PrintArrays arrSigns =
    Map.foldBack (fun var x a -> (sprintf "%-10s" (PrintSet (Set.toList x) "{"))+a) arrSigns ""

let PrintVars varSigns =
    Map.foldBack (fun var x a -> (sprintf "%5s" (ConvertIntSignToSign x))+a) varSigns ""

let PrintMemory (varSigns, arrSigns)=
    sprintf "%-22s %s \n" (PrintVars varSigns) (PrintArrays arrSigns)

let PrintNodeMemory q nam =
    sprintf "%s: \n%s" q (Set.foldBack (fun x a -> (PrintMemory x)+a) nam "")

let PrintAbstractMemory (absmem : Map<string , NodeAbsMem>) =
    let string1 = Set.foldBack (fun (map1, map2) a -> sprintf "%-22s  %s \n" (Map.foldBack (fun f _ a -> (sprintf "%5s" f)+a) map1 "") (Map.foldBack (fun f _ a -> (sprintf "%-10s" f)+a) map2 "")) (absmem.Item "qS") "" 
    let string2 = PrintNodeMemory "qS" (absmem.Item "qS")
    string1 + string2 + (Map.foldBack (fun e mem a ->  (sprintf "%s" (PrintNodeMemory e mem))+a) (absmem.Remove "qS") "")