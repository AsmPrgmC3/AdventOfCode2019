open IntcodeComputer


[<EntryPoint>]
let main _ = 
    let memory = IntcodeComputer.readInMemory "input1.txt"
    
    let rec permutations = function
        | [] -> [[]]
        | xs -> xs |> List.collect (fun x ->
            permutations (xs |> List.filter ((<>) x))
            |> List.map (fun perm -> x :: perm))
    
    permutations [0; 1; 2; 3; 4]
    |> Seq.map (List.fold (fun input phase -> List.last <| IntcodeComputer.executeProgram [phase; input] [] (Array.copy memory) 0) 0)
    |> Seq.max
    |> printfn "Maximum thrust: %d"
    
    0 // return an integer exit code
