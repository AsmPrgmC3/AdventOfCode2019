open System
open Utils.Utils

type Direction =
    | Up
    | Right
    | Down
    | Left

type Line =
    | HorizontalLine of y: int * startX: int * endX: int
    | VerticalLine of x: int * startY: int * endY: int

let doIntersect x smallY largeY y smallX largeX =
    if smallX <= x && x <= largeX &&
        smallY <= y && y <= largeY
    then Some (x, y)
    else None

let intersectLines = function
    | HorizontalLine (y, startX, endX) -> function
        | HorizontalLine _ -> None
        | VerticalLine (x, startY, endY) ->
            doIntersect x startY endY y startX endX
     | VerticalLine (x, startY, endY) -> function
         | VerticalLine _ -> None
         | HorizontalLine (y, startX, endX) ->
             doIntersect x startY endY y startX endX


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
    |> Seq.fold (fun ((_, (lx, ly)) :: _ as trace) -> function
        | Up, length -> (VerticalLine(lx, ly-length, ly), (lx, ly-length)) :: trace
        | Right, length -> (HorizontalLine(ly, lx, lx+length), (lx+length, ly)) :: trace
        | Down, length -> (VerticalLine(lx, ly, ly+length), (lx, ly+length)) :: trace
        | Left, length -> (HorizontalLine(ly, lx-length, lx), (lx-length, ly)) :: trace)
        [HorizontalLine(0, 0, 0), (0, 0); HorizontalLine(0, 0, 0), (0, 0)]
    |> Seq.map (fun (line, _) -> line)
    |> Seq.toList
    |> List.filter ((<>) (HorizontalLine(0, 0, 0)))


[<EntryPoint>]
let main _ = 
    
    let lines = Seq.toList <| readFileLines "input1.txt"
    
    let path1 = tracePath lines.[0]
    let path2 = tracePath lines.[1]
    
    path1
    |> Seq.collect (fun a ->
        List.choose (intersectLines a) path2)
    |> Seq.map (fun (x, y) -> abs(x) + abs(y))
    |> Seq.filter ((<>) 0)
    |> Seq.min
    |> printfn "Nearest crossover: %A"
    
    0 // return an integer exit code
