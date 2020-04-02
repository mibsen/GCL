module Interpret

open System.Collections.Generic

let rec interpret (Node(s)) (edgeList: Edge list) (remain: Edge list) (mem: Map<string, int>) =
    match remain with
    |[] -> raise (System.InvalidOperationException("failed in interpret"))
    |Edge(Node(s1), logic, Node(s2))::res when s = s1 -> try
                                                             let evalresult = evaluateEdge logic mem
                                                             match evalresult with 
                                                                | Some(memN,true) -> if s2 = "qE" then
                                                                                        s2, memN 
                                                                                     else   
                                                                                        interpret (Node(s2)) edgeList edgeList memN
                                                                | Some(memN,false) -> interpret (Node(s1)) edgeList res memN
                                                                | None -> s1, mem
                                                         with | :? KeyNotFoundException -> s1, mem
    | _::res -> interpret (Node(s)) edgeList res mem
 
and evaluateEdge (logic: Logic) (mem: Map<string, int>) =
    match logic with
        |AssignE(x, a) -> let value = evalArithExpr a mem
                          if mem.ContainsKey x then
                                Some(mem.Add(x, value), true)
                          else 
                                None
        | ArrayAssignE(x, a1, a2) ->  let index = evalArithExpr a1 mem
                                      let value = evalArithExpr a2 mem
                                      let name = x + (string index)
                                      if mem.ContainsKey name then
                                             Some(mem.Add(name, value), true)
                                      else 
                                             None
        | SkipE -> Some(mem, true)
        | BoolE(b) -> let result = evalBoolExpr b mem
                      if result then
                          Some(mem, true)
                      else
                          Some(mem, false)

and evalArithExpr a (mem: Map<string, int>) : int =
    match a with
        | N(n) -> n
        | X(x) -> mem.Item x
        | ArrayAccess(x,a) -> mem.Item (x+(string (evalArithExpr a mem)))
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
                           b1res && b2res
        | Or (b1 , b2) -> let b1res = evalBoolExpr b1 mem
                          let b2res = evalBoolExpr b2 mem
                          b1res || b2res
        | Not (b) ->  not (evalBoolExpr b mem)
        | Gt (a1, a2) -> (evalArithExpr a1 mem) > (evalArithExpr a2 mem)
        | Lt (a1, a2) -> (evalArithExpr a1 mem) < (evalArithExpr a2 mem) 
        | Le (a1, a2) -> (evalArithExpr a1 mem) <= (evalArithExpr a2 mem) 
        | Ge (a1, a2) -> (evalArithExpr a1 mem) >= (evalArithExpr a2 mem)
        | Eq (a1, a2) -> (evalArithExpr a1 mem) = (evalArithExpr a2 mem)
        | NotEq (a1, a2) -> (evalArithExpr a1 mem) <> (evalArithExpr a2 mem)