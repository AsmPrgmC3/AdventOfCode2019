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
    |> Seq.fold (fun ((lx, ly), trace: Set<int * int>) -> function
        | Up -> (lx, ly-1), trace.Add(lx, ly-1)
        | Right -> (lx+1, ly), trace.Add(lx+1, ly)
        | Down -> (lx, ly+1), trace.Add(lx, ly+1)
        | Left -> (lx-1, ly), trace.Add(lx-1, ly))
        ((0, 0), Set.empty)
    |> (fun (_, trace) -> trace)
    |> Set.remove (0, 0)


[<EntryPoint>]
let main _ = 
    
    let lines = Seq.toList <| readFileLines "input1.txt"
    
    let path1 = tracePath lines.[0]
    let path2 = tracePath lines.[1]
    
    path1
    |> Seq.filter (flip Set.contains path2)
    |> Seq.map (fun (x, y) -> abs(x) + abs(y))
    |> Seq.filter ((<>) 0)
    |> Seq.min
    |> printfn "Nearest crossover: %A"
    
    0 // return an integer exit code
