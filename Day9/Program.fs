open IntcodeComputer

[<EntryPoint>]
let main _ = 
    let prgm = IntcodeComputer.readInMemory "input1.txt"
    
    let memory = IntcodeComputer.newMemory prgm
    IntcodeComputer.runProgram [1L] [] memory 0
    |> List.last
    |> printfn "BOOST keycode: %d"
    
    let memory = IntcodeComputer.newMemory prgm
    IntcodeComputer.runProgram [2L] [] memory 0
    |> List.last
    |> printfn "Coordinates: %d"
    
    0 // return an integer exit code
