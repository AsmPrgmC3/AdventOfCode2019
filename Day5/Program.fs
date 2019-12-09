open IntcodeComputer


[<EntryPoint>]
let main _ = 
    let memory = IntcodeComputer.readInMemory "input1.txt"
    
    IntcodeComputer.runProgram [1L] [] (IntcodeComputer.newMemory memory) 0
    |> List.last
    |> printfn "Part1: %d"
    
    IntcodeComputer.runProgram [5L] [] (IntcodeComputer.newMemory memory) 0
    |> List.last
    |> printfn "Part2: %d"
    
    0 // return an integer exit code
