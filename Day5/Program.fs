open IntcodeComputer


[<EntryPoint>]
let main _ = 
    let memory = IntcodeComputer.readInMemory "input1.txt"
    
    IntcodeComputer.runProgram [1] [] (Array.copy memory) 0
    |> List.last
    |> printfn "Part1: %d"
    
    IntcodeComputer.runProgram [5] [] (Array.copy memory) 0
    |> List.last
    |> printfn "Part2: %d"
    
    0 // return an integer exit code
