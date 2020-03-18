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
    | N of int
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
    | N n-> sprintf "%i" n
    | X x-> sprintf "%s" x
    | ArrayAccess (x , a) ->  sprintf "ArrayAccess(%s, %s)" x (printA a)
    | Plus (a1 , a2) -> sprintf "Plus(%s, %s)" (printA a1) (printA a2)
    | Minus (a1, a2) -> sprintf "Minus(%s, %s)" (printA a1) (printA a2)
    | Multiply (a1, a2) -> sprintf "Multiply(%s, %s)" (printA a1) (printA a2)
    | Divide (a1, a2) -> sprintf "Divide(%s, %s)" (printA a1) (printA a2)
    | Pow (a1, a2) -> sprintf "Pow(%s, %s)" (printA a1) (printA a2)
    | UMinus (a) -> sprintf "UMinus(%s)" (printA a)

let rec printB b =
    match b with
    | Bool (b) -> string b
    | SAnd (b1 , b2) -> sprintf "SAnd(%s, %s)" (printB b1) (printB b2) 
    | SOr (b1 , b2) -> sprintf "SOr(%s, %s)" (printB b1) (printB b2) 
    | And (b1 , b2) -> sprintf "And(%s, %s)" (printB b1) (printB b2) 
    | Or (b1 , b2) -> sprintf "Or(%s, %s)" (printB b1) (printB b2) 
    | Not (b) ->  sprintf "Not(%s)" (printB b)
    | Gt (a1, a2) -> sprintf "Gt(%s, %s)" (printA a1) (printA a2) 
    | Lt (a1, a2) -> sprintf "Lt(%s, %s)" (printA a1) (printA a2) 
    | Le (a1, a2) -> sprintf "Le(%s, %s)" (printA a1) (printA a2) 
    | Ge (a1, a2) -> sprintf "Ge(%s, %s)" (printA a1) (printA a2) 
    | Eq (a1, a2) -> sprintf "Eq(%s, %s)" (printA a1) (printA a2)
    | NotEq (a1, a2) -> sprintf "NotEq(%s, %s)" (printA a1) (printA a2)

let rec print c= 
    match c with
    | Assign (x , a) -> sprintf "Assign(%s, %s)" x (printA a) 
    | ArrayAssign (x , a1 , a2) ->  sprintf "ArrayAssign(%s[%s], %s)" x (printA a1) (printA a2) 
    | Skip ->  sprintf "skip" 
    | Sequential (C1 , C2) -> sprintf "Seq(%s, \n %s)" (print C1) (print C2)  
    | If (gc) -> sprintf "If(%s)" (printGC gc) 
    | Do (gc) -> sprintf "Do(%s)" (printGC gc)
and printGC gc = 
    match gc with
    | Choice (b , C) -> sprintf "Choice(%s, %s)" (printB b) (print C)  
    | Conditional (gc1 , gc2) -> sprintf "Conditional(%s, %s)" (printGC gc1) (printGC gc2) 
