module Interpret

let rec evaluateEdges node es (map: Map<string, int>) =
    match es with
        | [] -> raise (System.InvalidOperationException("failed in evaluateEdges"))
        | [e]-> let evalresult = evaluateEdge e map
                match evalresult with 
                    | Some(nodeN, mapN) -> if nodeN.Name = "qE" then
                                              nodeN, mapN 
                                           else
                                              evaluateEdges nodeN node.Edges mapN
                    | None -> raise (System.InvalidOperationException(node.Name))
        | e::res -> let evalresult = evaluateEdge e map
                    match evalresult with 
                        | Some(nodeN, mapN) -> if nodeN.Name = "qE" then
                                                  nodeN, mapN 
                                               else
                                                  evaluateEdges nodeN node.Edges mapN
                        | None -> evaluateEdges node res map

and evaluateEdge e (map: Map<string, int>) =
    match e with
        |AssignE(x, a, node) -> let value = evalArithExpr a map
                                if map.ContainsKey x then
                                    printfn "%i" value
                                    Some(node, map.Add(x, value))
                                else 
                                    raise (System.InvalidOperationException("failed in AssignE"))
        | ArrayAssignE(x, a1, a2, node) ->  let index = evalArithExpr a1 map
                                            let value = evalArithExpr a2 map
                                            let name = x + (string index)
                                            if map.ContainsKey name then
                                                 Some(node, map.Add(name, value))
                                            else 
                                                 raise (System.InvalidOperationException("failed in ArrayAssignE"))
        | SkipE(node) -> Some(node, map)
        | ChoiceE(b, node) -> let result = evalBoolExpr b map
                              if result then
                                  Some(node, map)
                              else
                                  None


and evalArithExpr a (map: Map<string, int>) : int =
    match a with
        | N(n) -> n
        | X(x) -> map.Item x
        | ArrayAccess(x,a) -> map.Item (x+(string (evalArithExpr a map)))
        | Plus(a1, a2) -> (evalArithExpr a1 map) + (evalArithExpr a2 map)
        | Minus(a1, a2) -> (evalArithExpr a1 map) - (evalArithExpr a2 map)
        | Multiply(a1, a2) -> (evalArithExpr a1 map) * (evalArithExpr a2 map)
        | Divide(a1, a2) -> (evalArithExpr a1 map) / (evalArithExpr a2 map)
        | Pow(a1, a2) -> pown (evalArithExpr a1 map) (evalArithExpr a2 map)
        | UMinus(a) -> -(evalArithExpr a map)

and evalBoolExpr b (map: Map<string, int>) : bool =
    match b with
        | Bool (b) -> b
        | SAnd (b1 , b2) -> (evalBoolExpr b1 map) && (evalBoolExpr b2 map)
        | SOr (b1 , b2) -> (evalBoolExpr b1 map) || (evalBoolExpr b2 map)
        | And (b1 , b2) -> let b1res = evalBoolExpr b1 map
                           let b2res = evalBoolExpr b2 map
                           b1res && b2res
        | Or (b1 , b2) -> let b1res = evalBoolExpr b1 map
                          let b2res = evalBoolExpr b2 map
                          b1res || b2res
        | Not (b) ->  not (evalBoolExpr b map)
        | Gt (a1, a2) -> (evalArithExpr a1 map) > (evalArithExpr a2 map)
        | Lt (a1, a2) -> (evalArithExpr a1 map) < (evalArithExpr a2 map) 
        | Le (a1, a2) -> (evalArithExpr a1 map) <= (evalArithExpr a2 map) 
        | Ge (a1, a2) -> (evalArithExpr a1 map) >= (evalArithExpr a2 map)
        | Eq (a1, a2) -> (evalArithExpr a1 map) = (evalArithExpr a2 map)
        | NotEq (a1, a2) -> (evalArithExpr a1 map) <> (evalArithExpr a2 map)