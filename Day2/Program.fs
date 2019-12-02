open System
open Utils.Utils


let rec executeProgram (memory: int array) pos =
    match memory.[pos] with
    | 1 ->
        memory.[memory.[pos+3]] <- memory.[memory.[pos+1]] + memory.[memory.[pos+2]]
        executeProgram memory (pos + 4)
    | 2 ->
        memory.[memory.[pos+3]] <- memory.[memory.[pos+1]] * memory.[memory.[pos+2]]
        executeProgram memory (pos + 4)
    | 99 -> ()
    | x -> raise (new ArgumentException(sprintf "Invalid opcode: %d" x))


let valueAtStart (memory: int array) noun verb =
    let actualMemory = Array.copy memory
    actualMemory.[1] <- noun
    actualMemory.[2] <- verb
    executeProgram actualMemory 0
    actualMemory.[0]


let findNounVerb (memory: int array) target =
    let rec find noun verb =
        if valueAtStart memory noun verb = target
        then 100 * noun + verb
        else
            match verb with
            | 99 -> match noun with
                    | 99 -> -1
                    | _ -> find (noun + 1) 0
            | _ -> find noun (verb + 1)
    find 0 0


[<EntryPoint>]
let main _ =
    let memory =
        (readFileContent "input1.txt")
            .Split [| ',' |]
        |> Array.map int
    
    printfn "Left at start: %d" <| valueAtStart memory memory.[1] memory.[2]
    printfn "Noun/verb value for target: %d" <| findNounVerb memory (int <| readFileContent "input2.txt")
    
    0 // return an integer exit code
