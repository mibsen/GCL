module SignAnalysis

type NodeAbsMem = Set<Map<string, int> * Map<string, Set<int>>>

//Results on operations on signs
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
and AnalyseNode (Node(s)) (edgeList: Edge list) (absMem: Map<string, NodeAbsMem>) (workList: Node list) = 
    match edgeList with
        |[] -> absMem, workList
        |Edge(Node(s1), logic, Node(s2))::res when s = s1 -> let nodeAbsMem = LogicAnalysis (Set.toList (absMem.Item s1)) logic Set.empty                                                      
                                                             if not (absMem.ContainsKey s2) then
                                                                 AnalyseNode (Node(s1)) res (absMem.Add(s2, nodeAbsMem)) (Node(s2)::workList)
                                                             elif not (Set.isSubset nodeAbsMem (absMem.Item s2)) then
                                                                 let absMem2 = Set.union (absMem.Item s2) nodeAbsMem
                                                                 AnalyseNode (Node(s1)) res (absMem.Add(s2, absMem2)) (Node(s2)::workList)
                                                             else
                                                                 AnalyseNode (Node(s1)) res absMem workList
        |_::res -> AnalyseNode (Node(s)) res absMem workList

//Sign analysis is applied to each abstract memory the target node has
and LogicAnalysis nodeAbsMem (logic: Logic) (newAbsMem: NodeAbsMem) =
    match nodeAbsMem with
        |[] -> newAbsMem
        |absMem::res -> let result, b = SignAnalysis absMem logic
                        if b then
                            LogicAnalysis res logic (Set.union newAbsMem result)
                        else 
                            LogicAnalysis res logic newAbsMem

and SignAnalysis (varSigns, arrSigns) (logic: Logic) =
    match logic with
        | AssignE(x, a) -> let signs = (ArithAnalysis varSigns arrSigns a)
                           Set.foldBack (fun e a -> a.Add(varSigns.Add(x, e), arrSigns)) signs Set.empty, true
        | ArrayAssignE(x, a1, a2) ->let accesSigns = (ArithAnalysis varSigns arrSigns a1)
                                    if not (accesSigns.Contains(0) || accesSigns.Contains(1)) then //Can't access array is index is negative
                                        Set.empty.Add(varSigns, arrSigns), false
                                    else
                                        let asssigns = (ArithAnalysis varSigns arrSigns a2)
                                        
                                        //Signs are added to each abstract memory without removing any signs
                                        let set1 = Set.foldBack (fun e (a : NodeAbsMem) -> a.Add(varSigns, arrSigns.Add(x, (arrSigns.Item x).Add(e)))) asssigns Set.empty
                                        
                                        //Signs are removed from the abstract memory, corresponding to the case where the last variable of a sign in an array is replaced with another sign
                                        let reduceSigns = Set.foldBack (fun e a -> (Set.difference (arrSigns.Item x) (Set.empty.Add(e)))::a) (arrSigns.Item x) []
                                        let set2 = List.foldBack (fun (f : Set<int>) (b : NodeAbsMem) -> Set.union b (Set.foldBack (fun e a -> a.Add(varSigns, arrSigns.Add(x, (f.Add(e))))) asssigns Set.empty)) reduceSigns Set.empty
                                        
                                        (Set.union set1 set2), true
        | SkipE -> Set.empty.Add(varSigns, arrSigns), true
        | BoolE(b) -> let result = BoolAnalysis varSigns arrSigns b
                      if result.Contains true then
                        Set.empty.Add(varSigns, arrSigns), true
                      else 
                        Set.empty.Add(varSigns, arrSigns), false

//Corresponds to the analysis function A on page 52 in Formal Methods
and ArithAnalysis varSign arrSign a: Set<int> =
    match a with 
        | N(n) -> Set.empty.Add(sign n)
        | X(x) -> Set.empty.Add(varSign.Item x)
        | ArrayAccess(x,_) -> arrSign.Item x
        | Plus(a1, a2) -> SignOperation plusMap a1 a2 varSign arrSign
        | Minus(a1, a2) -> SignOperation minusMap a1 a2 varSign arrSign
        | Multiply(a1, a2) -> SignOperation multMap a1 a2 varSign arrSign
        | Divide(a1, a2) -> SignOperation divMap a1 a2 varSign arrSign
        | Pow(a1, a2) -> SignOperation powMap a1 a2 varSign arrSign
        | UMinus(a) -> Set.map (~-) (ArithAnalysis varSign arrSign a)    

//Method for getting the result of a sign operation
and SignOperation (map : Map<int, Map<int, int list>>) a1 a2 varSign arrSign =
    let a1signs = ArithAnalysis varSign arrSign a1
    let a2signs = ArithAnalysis varSign arrSign a2
    let a1maps = Set.foldBack (fun e a -> (map.Item e)::a) a1signs []
    Set.foldBack (fun e a -> Set.union (List.foldBack (fun (f:Map<int, int list>) b -> Set.union (Set.ofList (f.Item e)) b) a1maps Set.empty) a) a2signs Set.empty

//Corresponds to the analysis function B on page 53 in Formal Methods
and BoolAnalysis varSign arrSign b : Set<bool> =
    match b with
        | Bool (b) -> Set.empty.Add(b)
        | SAnd (b1 , b2) | And(b1, b2) -> BoolOperation (&&) b1 b2 varSign arrSign
        | SOr (b1 , b2) | Or(b1, b2) -> BoolOperation (||) b1 b2 varSign arrSign
        | Not (b) ->  Set.map (not) (BoolAnalysis varSign arrSign b)
        | Gt (a1, a2) -> Set.map (not) (SignComparison leMap a1 a2 varSign arrSign)
        | Lt (a1, a2) -> Set.map (not) (SignComparison geMap a1 a2 varSign arrSign)
        | Le (a1, a2) -> SignComparison leMap a1 a2 varSign arrSign
        | Ge (a1, a2) -> SignComparison geMap a1 a2 varSign arrSign
        | Eq (a1, a2) -> SignComparison eqMap a1 a2 varSign arrSign
        | NotEq (a1, a2) -> Set.map (not) (SignComparison eqMap a1 a2 varSign arrSign)

//Method for getting the result of a boolean operation
and BoolOperation op b1 b2 varSign arrSign =
    let b1signs = BoolAnalysis varSign arrSign b1
    let b2signs = BoolAnalysis varSign arrSign b2
    Set.foldBack (fun e a -> Set.union (Set.foldBack (fun f b -> Set.union (Set.empty.Add(op e f)) b) b2signs Set.empty) a) b1signs Set.empty

//Method for getting the result of a sign comparison
and SignComparison (map : Map<int, Map<int, bool list>>) a1 a2 varSign arrSign =
    let a1signs = ArithAnalysis varSign arrSign a1
    let a2signs = ArithAnalysis varSign arrSign a2
    let a1maps = Set.foldBack (fun e a -> (map.Item e)::a) a1signs []
    Set.foldBack (fun e a -> Set.union (List.foldBack (fun (f:Map<int, bool list>) b -> Set.union (Set.ofList (f.Item e)) b) a1maps Set.empty) a) a2signs Set.empty


//Below is code used to print the final result of the sign analysis
let convertIntSignToSign i =
    match i with
        | -1 -> "-"
        | 0 -> "0"
        | _ -> "+"
   
let rec printSet l a =
    match l with
        |[] -> a
        |[x] -> a + (convertIntSignToSign x) + "}"
        |x::xs -> printSet xs (a+(sprintf "%s, " (convertIntSignToSign x)))

let printArrays arrSigns =
    Map.foldBack (fun var x a -> (sprintf "%-10s" (printSet (Set.toList x) "{"))+a) arrSigns ""

let printVars varSigns =
    Map.foldBack (fun var x a -> (sprintf "%5s" (convertIntSignToSign x))+a) varSigns ""

let printMemory (varSigns, arrSigns)=
    sprintf "%-22s %s \n" (printVars varSigns) (printArrays arrSigns)

let printNodeMemory q nam =
    sprintf "%s: \n%s" q (Set.foldBack (fun x a -> (printMemory x)+a) nam "")

let printAbstractMemory (absmem : Map<string , NodeAbsMem>) =
    let string1 = Set.foldBack (fun (map1, map2) a -> sprintf "%-22s  %s \n" (Map.foldBack (fun f _ a -> (sprintf "%5s" f)+a) map1 "") (Map.foldBack (fun f _ a -> (sprintf "%-10s" f)+a) map2 "")) (absmem.Item "qS") "" 
    let string2 = printNodeMemory "qS" (absmem.Item "qS")
    string1 + string2 + (Map.foldBack (fun e mem a ->  (sprintf "%s" (printNodeMemory e mem))+a) (absmem.Remove "qS") "")