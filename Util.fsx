module Util

open System
open System.IO

let getPath (args : string array) = 
    try 
        args.[2]
     
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   

let getInput path=
    try 
        File.ReadAllText path
    with e -> 
        failwith ("could not read file: " + path)
        null

let getDeterministic (args : string array) =
    try 
        Boolean.Parse(args.[args.Length - 1])
    with e -> 
        false

let getInitVals (args : string array) =
    try 
        args.[3]
    with e -> 
        printfn "Insert Path to file"
        Console.ReadLine()   