module SecurityInput

type L =
    | Lattice of (L*L)
    | SubLattice of O
and O =
    | SubsetOrdering of (string*O)
    | FlowElement of (string*string)

type C =
    | ClassSequence of (C*C)
    | ClassAssign of (string*string)

type I =
    |Input of (L*C)

let join m1 m2 = 
    Map(Seq.concat [ (Map.toSeq m1) ; (Map.toSeq m2) ])

let rec latticeInterpret lattice =
    match lattice with
        | Lattice(l1, l2) -> Set.union (latticeInterpret l1) (latticeInterpret l2)
        | SubLattice(o) -> Set.empty.Add(orderingInterpret o)

and orderingInterpret ordering =
    match ordering with
        | SubsetOrdering(x, o) -> x::(orderingInterpret o)
        | FlowElement(x1, x2) -> [x1;x2]

let rec classificationInterpret classification =
    match classification with
        | ClassSequence(c1, c2) -> join (classificationInterpret c1) (classificationInterpret c2)
        | ClassAssign(x1, x2) -> Map.empty.Add(x1, x2)

let inputInterpret res = 
    match res with
        | Input(lattice, classification) -> (latticeInterpret lattice), (classificationInterpret classification)


