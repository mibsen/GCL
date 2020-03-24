module IP

open GCLAST
open PG
open System

///////////////////
// ORIGINAL SOURCE 
// http://www.fssnip.net/pc/title/ListShuffle

// -- | Auxiliary recursive drop function
let rec drop' i l1 p l2 =
  if List.length l2 = List.length l1 - 1 then l2
  else 
      let p = if p = i then p+1  else p 
      drop' i l1 (p+1) (List.append l2 [(List.item p l1)])

// -- | Removes the the nth element from list
let drop i l =
  drop' i l 0 []        

// -- | Pop one element from a specific list position
let pop i l =
  match l with
  | [] -> ([],[])
  | h :: t when i >= 0 -> 
      let e = [l.Item i]
      let ll = drop i l
      (e,ll)
  | _ -> ([],[])

// -- | Generate random number within interval a-b with seed s
let myrandom a b s =
  let r = System.Random(s)
  r.Next(a, b)

// -- | Shuffle' auxiliary
let rec shuffle' r l1 l2 =
  let len = List.length l1
  let t = pop r l1
  if len = 0 then l2
  else shuffle' (myrandom 0 (len-1) r) (snd t) (List.append l2 (fst t))  

// -- | Shuffle a list
let shuffle l =
  shuffle' (myrandom 0 ((List.length l)-1) 0) l []
 
/////////////////////////////



let getAValuetoString value =
    match value with 
    | N(i) -> string(i)
    | _ -> "ERROR NOT CORRECT Type stored as value"

let printMemory memory = 
    Map.iter ( fun key value -> printfn "%s: %s" key (string value)) memory


let rec evaluate b (memory:Map<string,int>) =
    match b with
    | Bool (bo) -> bo
    | SAnd (b1 , b2) -> (evaluate b1 memory) && (evaluate b2 memory) 
    | SOr (b1 , b2) -> (evaluate b1 memory) || (evaluate b2 memory) 
    | And (b1 , b2) -> (evaluate b1 memory) && (evaluate b2 memory) 
    | Or (b1 , b2) -> (evaluate b1 memory) || (evaluate b2 memory) 
    | Not (b) -> not(evaluate b memory)
    | Gt (a1, a2) -> (calculate a1 memory) > (calculate a2 memory)
    | Lt (a1, a2) -> (calculate a1 memory) < (calculate a2 memory)
    | Le (a1, a2) -> (calculate a1 memory) <= (calculate a2 memory)
    | Ge (a1, a2) -> (calculate a1 memory) >= (calculate a2 memory)
    | Eq (a1, a2) -> (calculate a1 memory) = (calculate a2 memory)
    | NotEq (a1, a2) -> (calculate a1 memory) <> (calculate a2 memory)
and calculate a (memory:Map<string,int>) =
    match a with
    | N (n)-> n
    | X(x)-> let found = memory.TryFind(x) 
             match found with
                | Some x -> x
                | None -> failwith (sprintf "Variable %s Not found in memory!" x)
    | ArrayAccess (x , a) ->  let found = memory.TryFind (sprintf "%s[%s]" x (string (calculate a memory)))
                              match found with
                              | Some x -> x
                              | None -> failwith (sprintf "Variable %s Not found in memory!" x)
    | Plus (a1 , a2) -> (calculate a1 memory) + (calculate a2 memory)
    | Minus (a1, a2) -> (calculate a1 memory) - (calculate a2 memory)
    | Multiply (a1, a2) -> (calculate a1 memory) * (calculate a2 memory)
    | Divide (a1, a2) -> (calculate a1 memory) / (calculate a2 memory)
    | Pow (a1, a2) ->   let z2 = (calculate a2 memory) 
                        if z2 > 0 
                        then failwith (sprintf "you cannot pow to a number below 0! z2=%i formula = %s" z2 (getAString a2))
                        else
                        pown (calculate a1 memory) z2
    | UMinus (a) -> - (calculate a memory)

let evaluateEdge edge memory =
    match edge with 
    | ChoiceE (b, _) -> evaluate b memory 
    | _ -> true


let rec run node (memory:Map<string,int>) = 
    printfn "Running: %s" node.Name
    // We need to select a node to follow 
    // OR we need to stop
    if node.Final then
        printfn "Status: terminated"
        printfn "Node: %s" node.Name
        printMemory memory
        true
    else
        // Lets take a random edge by shuffling the node edges
        let shuffled = shuffle node.Edges
    
        let found = List.tryFind (fun e -> evaluateEdge e memory) shuffled
        match found with
            | Some e -> performStep e memory
            | None -> printfn "status: stuck"
                      printfn "Node: %s" node.Name
                      printMemory
                      failwith "Stuck exception"           
and performStep edge memory =
    match edge with 
    | AssignE (x,a,node) -> run node (memory.Add(x,calculate a memory))
    | ArrayAssignE (x, a1, a2, node) -> run node (memory.Add(sprintf "%s[%s]" x (string (calculate a1 memory)),calculate a2 memory))
    | SkipE (node) -> run node memory
    | ChoiceE (b, node) -> run node memory
