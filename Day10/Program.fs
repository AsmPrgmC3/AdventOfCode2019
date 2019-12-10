open System
open Utils


let rec gcd a b =
    if a = 0 then b
    elif b = 0 then a
    elif a = b then a
    elif a > b then gcd (a - b) b
    else gcd a (b - a)


let asteroidsVisible x y asteroids =
        asteroids
        |> Seq.filter ((<>) (x, y))
        |> Seq.filter (fun (ax, ay) ->
            let dx, dy = ax - x, ay - y
            let steps = gcd (abs dx) (abs dy)
            let stepX, stepY = dx / steps, dy / steps
            
            seq { for step in 1 .. steps - 1 do yield x + step * stepX, y + step * stepY}
            |> Seq.exists (Utils.flip Set.contains asteroids)
            |> not)


let rec nthAsteroidShot x y asteroids n =
    let visible = Set <| asteroidsVisible x y asteroids
    if Set.count visible < n
    then
        nthAsteroidShot x y (asteroids - visible) (n - Set.count visible)
    else
        visible
        |> Seq.sortBy (fun (ax, ay) ->
            atan2 (double (x - ax)) (double (ay - y))
            |> fun a -> if abs (Math.PI - a) < 0.00000000001 then -a else a)
        |> Seq.skip (n - 1)
        |> Seq.head


[<EntryPoint>]
let main _ =
    let asteroids =
        Utils.readFileLines "input1.txt"
        |> array2D
        |> Array2D.mapi (fun top left cell -> (left, top), cell)
        |> Seq.cast<(int * int) * char>
        |> Seq.choose (fun (pos, cell) -> if cell = '#' then Some pos else None)
        |> Set
    
    let (baseX, baseY), visibleAsteroids =
        asteroids
        |> Seq.map (fun (x, y) -> (x, y), asteroidsVisible x y asteroids |> Seq.length)
        |> Seq.maxBy snd
    
    printfn "Maximum asteroids detectable: %d" visibleAsteroids
    
    
    nthAsteroidShot baseX baseY (Set <| asteroidsVisible baseX baseY asteroids) 200
    
    |> fun (x, y) -> x * 100 + y
    |> printfn "200th asteroid shot: %d"
    
    
    0 // return an integer exit code
