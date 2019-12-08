open System
open Utils


type Size = {rows: int; cols: int}


[<EntryPoint>]
let main _ =
    let ImageSize = {cols = 25; rows = 6}
    let LayerLength = ImageSize.cols * ImageSize.rows
    
    let imageData = Utils.readFileContent "input1.txt"
    let numLayers = imageData.Length / LayerLength
    
    let layers = List.init numLayers (fun i -> imageData.Substring(LayerLength * i, LayerLength))
    
    layers
    |> List.minBy (Utils.count '0')
    |> (fun layer ->
        let ones = Utils.count '1' layer
        let twos = Utils.count '2' layer
        ones * twos)
    |> printfn "Integrity check: %d"
    
    let image = Array2D.init ImageSize.rows ImageSize.cols (fun row col ->
        layers
        |> List.find (fun layer -> layer.[row * ImageSize.cols + col] <> '2')
        |> (fun layer -> layer.[row * ImageSize.cols + col]))
    
    image
    |> Array2D.iteri (fun _ col color ->
        if col = 0 then printfn ""
        Console.BackgroundColor <- match color with
                                   | '0' -> ConsoleColor.Black
                                   | '1' -> ConsoleColor.White
                                   | _ -> ConsoleColor.Red
        printf "  ")
    
    0 // return an integer exit code
