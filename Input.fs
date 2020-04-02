module Input

type A =
    | VarInit of (string * int)
    | ArrayInit of (string * AL)
    | Sequence of (A * A)
and AL = 
    | ArraySequence of (int * AL)
    | Num of int


let rec printA a = 
    match a with 
        |VarInit(x, a) -> sprintf "VarInit(%s, %i)" x a 
        |ArrayInit(x, al) -> sprintf "ArrayInit(%s, %s)" x (printAL al) 
        |Sequence(a1, a2) -> (printA a1) + (printA a2)

and printAL a =
    match a with 
        |ArraySequence(a, al) -> sprintf "ArraySequence(%i, %s)" a (printAL al)
        |Num(a) -> string a