module GCLAST
   
type C = 
    | Assign of (x * a)
    | ArrayAssign of (x * a * a)
    | Skip
    | Sequential of (C * C)
    | If of (gc)
    | Do of (gc)

and gc = 
    | Choice of (b * C)
    | Conditional of (gc * gc)

and  a = 
    | N of float
    | X of x
    | ArrayAccess of (x * a)
    | Plus of (a * a)
    | Minus of (a * a)
    | Multiply of (a * a)
    | Divide of (a * a)
    | Pow of (a * a)
    | UMinus of (a)

and x = string

and b = 
    | Bool of bool
    | SAnd of (b * b)
    | SOr of (b * b)
    | And of (b * b)
    | Or of (b * b)
    | Not of (b)
    | Gt of (a * a)
    | Lt of (a * a)
    | Le of (a * a)
    | Ge of (a * a)
    | Eq of (a * a)
    | NotEq of (a * a)
 
let rec printA a =
    match a with
    | N n-> sprintf "%f" n
    | X x-> sprintf "%s" x
    | ArrayAccess (x , a) ->  sprintf "%s[%s]" x (printA a)
    | Plus (a1 , a2) -> sprintf "%s + %s" (printA a1) (printA a2)
    | Minus (a1, a2) -> sprintf "%s - %s" (printA a1) (printA a2)
    | Multiply (a1, a2) -> sprintf "%s * %s" (printA a1) (printA a2)
    | Divide (a1, a2) -> sprintf "%s / %s" (printA a1) (printA a2)
    | Pow (a1, a2) -> sprintf "%s^%s" (printA a1) (printA a2)
    | UMinus (a) -> sprintf "-%s" (printA a)

let rec printB b =
    match b with
    | Bool (b) -> string b
    | SAnd (b1 , b2) -> sprintf "%s & %s" (printB b1) (printB b2) 
    | SOr (b1 , b2) -> sprintf "%s | %s" (printB b1) (printB b2) 
    | And (b1 , b2) -> sprintf "%s && %s" (printB b1) (printB b2) 
    | Or (b1 , b2) -> sprintf "%s || %s" (printB b1) (printB b2) 
    | Not (b) ->  sprintf "!%s" (printB b)
    | Gt (a1, a2) -> sprintf "%s < %s" (printA a1) (printA a2) 
    | Lt (a1, a2) -> sprintf "%s > %s" (printA a1) (printA a2) 
    | Le (a1, a2) -> sprintf "%s <= %s" (printA a1) (printA a2) 
    | Ge (a1, a2) -> sprintf "%s >= %s" (printA a1) (printA a2) 
    | Eq (a1, a2) -> sprintf "%s = %s" (printA a1) (printA a2)
    | NotEq (a1, a2) -> sprintf "%s != %s" (printA a1) (printA a2)

let rec printC c i= 
    match c with
    | Assign (x , a) -> sprintf "%s%s := %s"i x (printA a) 
    | ArrayAssign (x , a1 , a2) ->  sprintf "%s %s[%s] := %s" i x (printA a1) (printA a2) 
    | Skip ->  sprintf "skip" 
    | Sequential (C1 , C2) -> sprintf " %s; \n %s" (printC C1 i) (printC C2 i)  
    | If (gc) -> sprintf "%sif %s \n%sfi" i (printGC gc (i)) i
    | Do (gc) -> sprintf "%sdo %s \n%sod" i (printGC gc (i+"    ")) i  
and printGC gc i = 
    match gc with
    | Choice (b , C) -> sprintf "%s%s -> %s" i (printB b) (printC C i)  
    | Conditional (gc1 , gc2) -> sprintf " %s \n[] %s" (printGC gc1 i) (printGC gc2 i) 
    
let print c = printC c ""