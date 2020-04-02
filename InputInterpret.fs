module InputInterpret

let rec inputInterpret ast (mem: Map<string, int>): Map<string, int>=
    match ast with
       |VarInit(x, a) -> mem.Add(x,a)
       |ArrayInit(x, al) -> inputInterpretArray x al mem 0
       |Sequence(a1, a2) -> inputInterpret a2 (inputInterpret a1 mem)

and inputInterpretArray x al mem i =
    match al with 
        |ArraySequence(a, al2) -> (inputInterpretArray x al2 mem (i+1)).Add(x+(string i), a)
        |Num(a) -> mem.Add(x+(string i), a)