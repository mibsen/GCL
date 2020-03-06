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
    | NotEq of (a * a);;
 
    
