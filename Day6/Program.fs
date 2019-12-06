open System
open Utils.Utils


let numOrbits orbits object =
    let rec countOrbits currentObject orbitsSoFar =
        if Map.containsKey currentObject orbits
        then countOrbits orbits.[currentObject] (orbitsSoFar + 1)
        else orbitsSoFar
    
    countOrbits object 0


let findRoute orbits fromObject toObject =
    let keys =
        orbits
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.toList
    
    let rec search lastObject currentLength currentObject =
        if currentObject = toObject then
            currentLength
        else
            let lengths =
                keys
                |> Seq.filter (flip Map.find orbits >> (=) currentObject)
                |> Seq.append (if orbits.ContainsKey(currentObject) then [orbits.[currentObject]] else [])
                |> Seq.filter ((<>) lastObject)
                |> Seq.map (search currentObject (currentLength + 1))
                |> Seq.toList
            if lengths.Length = 0 then Int32.MaxValue else List.min lengths
    
    search fromObject -2 fromObject


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
    
    printfn "Jumps required: %d" <| findRoute orbits "YOU" "SAN"
    
    0 // return an integer exit code
