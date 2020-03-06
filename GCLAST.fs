module GCLAST
   

type x = string
and  a = 
    | N of float
    | X of x
    | Plus of (a * a)
    | Minus of (a * a)
    | Multiply of (a * a)
    | Divide of (a * a)
    | Pow of (a * a)
    | UPlus of (a)
    | UMinus of (a)


and b = 
    | Bool of bool
    | AND of (b * b)
    | OR of (b * b)
    | NOT of (b)


and C = 
        | Assign of (x * a)
        | Skip
        | IF of (gc)
        | DO of (gc)
and gc = 
    | Choise of (b * C);;
    
