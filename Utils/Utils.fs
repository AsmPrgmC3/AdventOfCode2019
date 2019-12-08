module Utils.Utils

open System
open System.IO

let flip f y x = f x y


let count value sequence =
    sequence
    |> Seq.filter ((=) value)
    |> Seq.length


let readFileLines =
    File.ReadLines

let readFileContent =
    File.ReadAllText


let rec inputNumber () =
    let input = Console.ReadLine()
    let success, number = Int32.TryParse input
    if success then number else inputNumber ()


[<Struct>]
type OptionalBuilder =
    member this.Bind(opt, binder) =
        match opt with
        | Some value -> binder value
        | None -> None
        
        
    member this.Return(value) =
        Some value
        

    member this.Zero() =
        None

let optional = new OptionalBuilder()
