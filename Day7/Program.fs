open IntcodeComputer
open Utils

[<EntryPoint>]
let main _ = 
    let memory = IntcodeComputer.readInMemory "input1.txt"
    
    let rec permutations = function
        | [] -> [[]]
        | xs -> xs |> List.collect (fun x ->
            permutations (xs |> List.except [x])
            |> List.map (fun perm -> x :: perm))
    
    permutations [0; 1; 2; 3; 4]
    |> Seq.map (List.fold (fun input phase -> List.last <| IntcodeComputer.runProgram [phase; input] [] (Array.copy memory) 0) 0)
    |> Seq.max
    |> printfn "Maximum thrust: %d"
    
    permutations [5; 6; 7; 8; 9]
    |> Seq.map (fun perm ->
        let mem1 = Array.copy memory
        let mutable pos1 = 0
        let mem2 = Array.copy memory
        let mutable pos2 = 0
        let mem3 = Array.copy memory
        let mutable pos3 = 0
        let mem4 = Array.copy memory
        let mutable pos4 = 0
        let mem5 = Array.copy memory
        let mutable pos5 = 0
        
        let mutable result = 0
        let mutable inputExtras = List.map (fun x -> [x]) perm
        let mutable doContinue = true
        while doContinue do
            Utils.optional {
                let! out1, newPos1, _ = IntcodeComputer.runUntilOutput (inputExtras.[0] @ [result]) mem1 pos1
                let! out2, newPos2, _ = IntcodeComputer.runUntilOutput (inputExtras.[1] @ [out1]) mem2 pos2
                let! out3, newPos3, _ = IntcodeComputer.runUntilOutput (inputExtras.[2] @ [out2]) mem3 pos3
                let! out4, newPos4, _ = IntcodeComputer.runUntilOutput (inputExtras.[3] @ [out3]) mem4 pos4
                let! out5, newPos5, _ = IntcodeComputer.runUntilOutput (inputExtras.[4] @ [out4]) mem5 pos5
                
                pos1 <- newPos1
                pos2 <- newPos2
                pos3 <- newPos3
                pos4 <- newPos4
                pos5 <- newPos5
                
                result <- out5
                
                inputExtras <- List.replicate 5 []
                
                return result
            } |> (fun optionalResult -> doContinue <- optionalResult <> None)
        result)
    |> Seq.max
    |> printfn "Maximum thrust in feedback mode: %d"
    
    0 // return an integer exit code
