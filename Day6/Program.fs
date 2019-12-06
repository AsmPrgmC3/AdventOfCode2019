open Utils.Utils


let numOrbits orbits object =
    let rec countOrbits currentObject orbitsSoFar =
        if Map.containsKey currentObject orbits
        then countOrbits orbits.[currentObject] (orbitsSoFar + 1)
        else orbitsSoFar
    
    countOrbits object 0


[<EntryPoint>]
let main _ = 
    let orbits =
        readFileLines("input1.txt")
        |> Seq.map (fun line -> line.Split(')'))
        |> Seq.map (fun [| around; object |] -> object, around)
        |> Map.ofSeq
    
    orbits
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.map (fun object -> numOrbits orbits object)
    |> Seq.sum
    |> printfn "Number of orbits: %d"
    
    0 // return an integer exit code
