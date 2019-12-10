open Utils


let rec gcd a b =
    if a = 0 then b
    elif b = 0 then a
    elif a = b then a
    elif a > b then gcd (a - b) b
    else gcd a (b - a)


[<EntryPoint>]
let main _ =
    let asteroids =
        Utils.readFileLines "input1.txt"
        |> array2D
        |> Array2D.mapi (fun top left cell -> (left, top), cell)
        |> Seq.cast<(int * int) * char>
        |> Seq.choose (fun (pos, cell) -> if cell = '#' then Some pos else None)
        |> Set
    
    asteroids
    |> Seq.map (fun (x, y) ->
        asteroids
        |> Seq.filter ((<>) (x, y))
        |> Seq.filter (fun (ax, ay) ->
            let dx, dy = ax - x, ay - y
            let steps = gcd (abs dx) (abs dy)
            let stepX, stepY = dx / steps, dy / steps
            
            seq { for step in 1 .. steps - 1 do yield x + step * stepX, y + step * stepY}
            |> Seq.exists (Utils.flip Set.contains asteroids)
            |> not)
        |> Seq.length)
    |> Seq.max
    |> printfn "Maximum asteroids detectable: %d"
    
    0 // return an integer exit code
