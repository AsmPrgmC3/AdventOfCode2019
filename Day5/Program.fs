open IntcodeComputer
open Utils.Utils


[<EntryPoint>]
let main _ = 
    let memory = IntcodeComputer.readInMemory "input1.txt"
    
    IntcodeComputer.executeProgram [1] [] (Array.copy memory) 0
    |> List.last
    |> printfn "Part1: %d"
    
    IntcodeComputer.executeProgram [5] [] (Array.copy memory) 0
    |> List.last
    |> printfn "Part2: %d"
    
    0 // return an integer exit code
