open System
open Utils.Utils

type Direction =
    | Up
    | Right
    | Down
    | Left


let tracePath (line: string) =
    line.Split(',')
    |> Seq.map (fun step ->
        let dir, length = step.[0], int (step.Substring(1))
        match dir with
        | 'U' -> Up, length
        | 'R' -> Right, length
        | 'D' -> Down, length
        | 'L' -> Left, length
        | x -> raise (new ArgumentException(sprintf "'%c' is not a valid direction" x)))
    |> Seq.collect (fun (dir, length) ->
        Seq.replicate length dir)
    |> Seq.fold (fun ((lx, ly), trace: Set<int * int>, map: Map<int * int, int>, step) dir ->
        let next = match dir with
                   | Up -> lx, ly-1
                   | Right -> lx+1, ly
                   | Down -> lx, ly+1
                   | Left -> lx-1, ly
        next, trace.Add next, map.Add(next, step+1), step+1)
        ((0, 0), Set.empty, Map.empty, 0)
    |> (fun (_, trace, distanceMap, _) -> trace.Remove(0, 0), distanceMap)


[<EntryPoint>]
let main _ = 
    
    let lines = Seq.toList <| readFileLines "input1.txt"
    
    let path1, distances1 = tracePath lines.[0]
    let path2, distances2 = tracePath lines.[1]
    
    let crossOvers = Set.filter (flip Set.contains path2) path1
    
    crossOvers
    |> Seq.map (fun (x, y) -> abs(x) + abs(y))
    |> Seq.min
    |> printfn "Nearest crossover: %A"
    
    crossOvers
    |> Seq.map (fun point -> distances1.[point] + distances2.[point])
    |> Seq.min
    |> printfn "First crossover: %A"
    
    0 // return an integer exit code
