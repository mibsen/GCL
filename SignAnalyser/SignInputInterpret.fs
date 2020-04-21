module SignInputInterpret

let convertSignToIntSign s =
    match s with
        | "-" -> -1
        | "0" -> 0
        | _ -> 1

let rec inputInterpret ast ((varMap, arrMap): Map<string, int> * Map<string, Set<int>>): Map<string, int> * Map<string, Set<int>>=
    match ast with
       |SignVarInit(x, a) -> varMap.Add(x, (convertSignToIntSign a)), arrMap
       |SignArrayInit(x, al) -> varMap, arrMap.Add(x, (inputInterpretArray al Set.empty))
       |Sequence(a1, a2) -> inputInterpret a2 (inputInterpret a1 (varMap, arrMap))

and inputInterpretArray al set =
    match al with 
        |SignArraySequence(a, al2) -> inputInterpretArray al2 (set.Add(convertSignToIntSign a))
        |Sign(a) -> set.Add(convertSignToIntSign a)