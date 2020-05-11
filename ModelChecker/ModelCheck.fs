module ModelCheck

open System.Collections.Generic

// type a = 
//     | N of int
//     | X of x
//     | ArrayAccess of (x * a)
//     | Plus of (a * a)
//     | Minus of (a * a)
//     | Multiply of (a * a)
//     | Divide of (a * a)
//     | Pow of (a * a)
//     | UMinus of (a)

// and x = string

// and b = 
//     | Bool of bool
//     | SAnd of (b * b)
//     | SOr of (b * b)
//     | And of (b * b)
//     | Or of (b * b)
//     | Not of (b)
//     | Gt of (a * a)
//     | Lt of (a * a)
//     | Le of (a * a)
//     | Ge of (a * a)
//     | Eq of (a * a)
//     | NotEq of (a * a)

// type Node =
//     | Node of (string)
// and Edge =
//     | Edge of (Node*Logic*Node) 
// and Logic =
//     | AssignE of (x*a)
//     | ArrayAssignE of (x * a * a)
//     | SkipE
//     | BoolE of (b)

type State = Node*Map<string, int>

//Pseudo-algorithm from the assignment
let rec CheckModel (toExplore: State list) (visited: Set<State>) (stuckStates: Set<State>) (edgeList: Edge list)=
    match toExplore with
        | [] -> stuckStates
        | x::xs when not (Set.contains x visited) -> let (reached, stuck) = Reach1 x edgeList Set.empty Set.empty true
                                                     CheckModel (xs @ (Set.toList reached)) (visited.Add(x)) (Set.union stuck stuckStates) edgeList
        | _::xs -> CheckModel xs visited stuckStates edgeList

//Implementation of Reach1 from Formal Methods page 79
and Reach1 (Node(s), mem: Map<string, int>) (remain: Edge list) (reached: Set<State>) (stuckStates: Set<State>) (isStuck: bool) =
    match remain with
        |[] -> if isStuck then //If no transision has been found from this state, then the state is stuck
                  reached, stuckStates.Add(Node(s), mem)
               else
                  reached, stuckStates
        |Edge(Node(s1), logic, Node(s2))::res when s = s1 -> try
                                                                 let evalresult = evaluateEdge logic mem
                                                                 match evalresult with 
                                                                    | Some(memN,true) -> Reach1 (Node(s), mem) res (reached.Add(Node(s2), memN)) stuckStates false //This means the state is not stuck
                                                                    | Some(memN,false) -> Reach1 (Node(s), mem) res reached stuckStates isStuck //Bool edge that evaluated to false
                                                                    | None -> Reach1 (Node(s), mem) res reached stuckStates isStuck //Accessed invalied variable, equivalent to not having a transition from the state
                                                              with | :? KeyNotFoundException -> Reach1 (Node(s), mem) res reached stuckStates isStuck //Accessed invalied variable
        | _::res -> Reach1 (Node(s), mem) res reached stuckStates isStuck

//Below is logic for evaluating edge logic
and evaluateEdge (logic: Logic) (mem: Map<string, int>) =
    match logic with
        | AssignE(x, a) -> let value = evalArithExpr a mem
                           if mem.ContainsKey x then
                                Some(mem.Add(x, value), true)
                           else 
                                None
        | ArrayAssignE(x, a1, a2) ->  let index = evalArithExpr a1 mem
                                      let value = evalArithExpr a2 mem
                                      let name = sprintf "%s[%i]" x index
                                      if mem.ContainsKey name then
                                             Some(mem.Add(name, value), true)
                                      else 
                                             None
        | SkipE -> Some(mem, true)
        | BoolE(b) -> Some(mem, evalBoolExpr b mem)

and evalArithExpr a (mem: Map<string, int>) : int =
    match a with
        | N(n) -> n
        | X(x) -> mem.Item x
        | ArrayAccess(x,a) -> mem.Item (sprintf "%s[%i]" x (evalArithExpr a mem))
        | Plus(a1, a2) -> (evalArithExpr a1 mem) + (evalArithExpr a2 mem)
        | Minus(a1, a2) -> (evalArithExpr a1 mem) - (evalArithExpr a2 mem)
        | Multiply(a1, a2) -> (evalArithExpr a1 mem) * (evalArithExpr a2 mem)
        | Divide(a1, a2) -> (evalArithExpr a1 mem) / (evalArithExpr a2 mem)
        | Pow(a1, a2) -> pown (evalArithExpr a1 mem) (evalArithExpr a2 mem)
        | UMinus(a) -> -(evalArithExpr a mem)

and evalBoolExpr b (mem: Map<string, int>) : bool =
    match b with
        | Bool (b) -> b
        | SAnd (b1 , b2) -> (evalBoolExpr b1 mem) && (evalBoolExpr b2 mem)
        | SOr (b1 , b2) -> (evalBoolExpr b1 mem) || (evalBoolExpr b2 mem)
        | And (b1 , b2) -> let b1res = evalBoolExpr b1 mem
                           let b2res = evalBoolExpr b2 mem
                           (b1res && b2res) || (b2res && b1res)
        | Or (b1 , b2) -> let b1res = evalBoolExpr b1 mem
                          let b2res = evalBoolExpr b2 mem
                          (b1res || b2res) && (b2res || b1res)
        | Not (b) ->  not (evalBoolExpr b mem)
        | Gt (a1, a2) -> (evalArithExpr a1 mem) > (evalArithExpr a2 mem)
        | Lt (a1, a2) -> (evalArithExpr a1 mem) < (evalArithExpr a2 mem) 
        | Le (a1, a2) -> (evalArithExpr a1 mem) <= (evalArithExpr a2 mem) 
        | Ge (a1, a2) -> (evalArithExpr a1 mem) >= (evalArithExpr a2 mem)
        | Eq (a1, a2) -> (evalArithExpr a1 mem) = (evalArithExpr a2 mem)
        | NotEq (a1, a2) -> (evalArithExpr a1 mem) <> (evalArithExpr a2 mem)