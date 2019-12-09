open IntcodeComputer

[<EntryPoint>]
let main _ = 
    
    let memory = IntcodeComputer.newMemory (IntcodeComputer.readInMemory "input1.txt")
    IntcodeComputer.runProgram [1L] [] memory 0
    |> List.last
    |> printfn "BOOST keycode: %d"
    
    0 // return an integer exit code
