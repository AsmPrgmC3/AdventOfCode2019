open Utils.Utils


[<EntryPoint>]
let main _ = 
    let [| lower; upper|] = (readFileContent "input1.txt")
                                .Split('-')
                                |> Array.take 2
                                |> Array.map int
    
    seq { for num in lower..upper -> num }
    |> Seq.map string
    |> Seq.map Seq.pairwise
    |> Seq.filter (Seq.exists (fun (a, b) -> a = b))
    |> Seq.filter (not << Seq.exists (fun (a, b) -> b < a))
    |> Seq.length
    |> printfn "Possible passwords: %d"
    
    0 // return an integer exit code
