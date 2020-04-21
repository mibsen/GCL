module SignInput

type A =
    | SignVarInit of (string * string)
    | SignArrayInit of (string * AL)
    | Sequence of (A * A)
and AL = 
    | SignArraySequence of (string * AL)
    | Sign of string


let rec printA a = 
    match a with 
        |SignVarInit(x, a) -> sprintf "SignVarInit(%s, %s)" x a 
        |SignArrayInit(x, al) -> sprintf "SignArrayInit(%s, %s)" x (printAL al) 
        |Sequence(a1, a2) -> (printA a1) + (printA a2)

and printAL a =
    match a with 
        |SignArraySequence(a, al) -> sprintf "SignArraySequence(%s, %s)" a (printAL al)
        |Sign(a) -> a