open System
open Utils.Utils


let rec executeProgram (prgm: int array) pos =
    match prgm.[pos] with
    | 1 ->
        prgm.[prgm.[pos+3]] <- prgm.[prgm.[pos+1]] + prgm.[prgm.[pos+2]]
        executeProgram prgm (pos + 4)
    | 2 ->
        prgm.[prgm.[pos+3]] <- prgm.[prgm.[pos+1]] * prgm.[prgm.[pos+2]]
        executeProgram prgm (pos + 4)
    | 99 -> ()
    | x -> raise (new ArgumentException(sprintf "Invalid opcode: %d" x))


[<EntryPoint>]
let main _ =
    let values =
        (readFileContent "input1.txt")
            .Split [| ',' |]
        |> Array.map int
    
    
    executeProgram values 0
    
    printfn "Left at start: %d" values.[0]
    
    0 // return an integer exit code
