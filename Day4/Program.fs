open Utils.Utils


[<EntryPoint>]
let main _ = 
    let [| lower; upper|] = (readFileContent "input1.txt")
                                .Split('-')
                                |> Array.take 2
                                |> Array.map int
    
    let part1Numbers = seq { for num in lower..upper -> num }
                      |> Seq.map string
                      |> Seq.map (fun num -> num, Seq.pairwise num)
                      |> Seq.filter (fun (_, pairs) -> pairs |> Seq.exists (fun (a, b) -> a = b))
                      |> Seq.filter (fun (_, pairs) -> pairs |> (not << Seq.exists (fun (a, b) -> b < a)))
                      |> Seq.map (fun (num, _) -> num)
    
    part1Numbers
    |> Seq.length
    |> printfn "Possible passwords (part 1): %d"
    
    part1Numbers
    |> Seq.map (Seq.countBy id)
    |> Seq.filter (Seq.exists ((=) 2 << snd))
    |> Seq.length
    |> printfn "Possible passwords (part 2): %d"
    
    0 // return an integer exit code
