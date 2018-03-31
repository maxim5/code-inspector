// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Program
open Redcode
open VM
open Parser
open System
open System.IO

let parseAndAddWarrior core name = 
    try
        let lines = File.ReadAllLines name |> Array.toList
        let warrior = parseWarrior lines
        printProgram warrior.Code
        addWarrior core warrior
    with
    | :? FileNotFoundException as ex -> failwithf "File %s not found.\n" name
    | _ as ex -> failwithf "Error opening file %s. Exception: %A \n" name ex


let rec coreLoop core =
    if core.Processes.Length = 0 then 0 else
        printCore core
        let core' = executeStep core
        let key = Console.ReadKey true 
        if key.KeyChar = 'q' then 0 else coreLoop core'

[<EntryPoint>]
let main argv = 
    let core = initialCore 24
    let populated_core = 
        List.fold (fun core' prog -> parseAndAddWarrior core' prog) core
            (Array.toList argv)

    coreLoop populated_core
    //printfn "\n\nFinal core:\n%s" (showCore final_core)
    