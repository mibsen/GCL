module SecurityAnalysis


//Equivalent to the fv function found on page 64 in Formal Methods. Collects free variables and array names
//from arithmetic expressions
let rec ArithAnalysis (a : a) =
    match a with 
        | N(n) -> Set.empty
        | X(x) -> Set.empty.Add(x)
        | ArrayAccess(x,a) -> (ArithAnalysis a).Add(x)
        | Plus(a1, a2) | Minus(a1, a2) | Multiply(a1, a2) | Divide(a1, a2) | Pow(a1, a2) -> Set.union (ArithAnalysis a1) (ArithAnalysis a2)
        | UMinus(a) -> ArithAnalysis a

//Collects free variables and array names from boolean expressions
and BoolAnalysis (b : b) =
    match b with
        | Bool (b) -> Set.empty
        | SAnd (b1 , b2) | And(b1, b2) | SOr (b1 , b2) | Or(b1, b2) -> Set.union (BoolAnalysis b1) (BoolAnalysis b2)
        | Not (b) -> BoolAnalysis b
        | Gt (a1, a2) | Lt (a1, a2) | Le (a1, a2) | Ge (a1, a2) | Eq (a1, a2) | NotEq (a1, a2) -> Set.union (ArithAnalysis a1) (ArithAnalysis a2)

//Works as a combination of edges_s from page 64 in Formal Methods and a method for returning all the flows of a program. 
//Implicit flows are passed on and flow relations are calculated.
let rec buildC c implicitFlows = 
    match c with
        | Assign(x, a) -> let explicitFlows = ArithAnalysis a
                          FlowAnalysis x (Set.union explicitFlows implicitFlows)
        | ArrayAssign(x, a1, a2) -> let explicitFlows = Set.union (ArithAnalysis a1) (ArithAnalysis a2)
                                    FlowAnalysis x (Set.union explicitFlows implicitFlows)
        | Skip -> Set.empty
        | Sequential(C1, C2) ->  let edges1 = buildC C1 implicitFlows
                                 let edges2 = buildC C2 implicitFlows
                                 Set.union edges1 edges2
        | If(gc) -> let (flows, prev) = buildGC gc (Bool(false)) implicitFlows        
                    flows 
        | Do(gc) -> let (flows, prev) =buildGC gc (Bool(false)) implicitFlows
                    flows
                    

and buildGC gc (prev: b) implicitFlows = 
    match gc with
        | Choice(b , C) -> let implicitFlows2 = Set.union (BoolAnalysis b) (BoolAnalysis prev)
                           let flows = buildC C (Set.union implicitFlows implicitFlows2)
                           flows, Or(b, prev)  
        | Conditional (gc1 , gc2) -> let (edges, prev2) = buildGC gc1 prev implicitFlows
                                     let (edges2, prev3) = buildGC gc2 prev2 implicitFlows
                                     Set.union edges edges2, prev3
and gcDone gc  = 
    match gc with
        | Choice (b, _) -> b
        | Conditional (gc1 , gc2) -> Or((gcDone gc2), (gcDone gc1))

//Returns all flow relations of a variable with the given flows
and FlowAnalysis x flows =
    Set.foldBack (fun e a -> a.Add(e,x)) flows Set.empty

//Checks if a flow is allowed. A flow is allowed if the index of the clss1 is lower or equal to the index of clss2 in the list
//i.e. clss1 occurs before or at the same point in the list
let rec flowAllowed clss1 clss2 (lattice: string list list) = 
    match lattice with
        | [] -> false
        | x::xs when List.contains clss1 x && List.contains clss2 x -> List.findIndex (fun e -> e = clss1) x <= List.findIndex (fun e -> e = clss2) x
        | _::xs -> flowAllowed clss1 clss2 xs

//The two methods below iterate of every possible combination of variables and return all allowed flows
let varFlows (var: string) (clss: string) (classification: Map<string, string>) (lattice: Set<string list>) =
     Map.foldBack (fun var2 clss2 (set: Set<string*string>) -> if flowAllowed clss clss2 (Set.toList lattice) then set.Add(var, var2) else set) classification Set.empty

let computeAllowedFlows (classification: Map<string, string>) lattice  =
    Map.foldBack (fun var clss set -> Set.union set (varFlows var clss classification lattice)) classification Set.empty