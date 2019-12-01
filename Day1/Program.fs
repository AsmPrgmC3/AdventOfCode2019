open Utils.Utils

let calculateFuel =
    flip (/) 3
    >> flip (-) 2

[<EntryPoint>]
let main _ = 
    let lines = readFile "input1.txt"
    lines
    |> Seq.map int
    |> Seq.map calculateFuel
    |> Seq.sum
    |> printfn "Required Fuel: %d"
    
    0 // return an integer exit code
