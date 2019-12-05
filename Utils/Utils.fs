module Utils.Utils

open System
open System.IO

let flip f y x = f x y

let readFileLines =
    File.ReadLines

let readFileContent =
    File.ReadAllText


let rec inputNumber () =
    let input = Console.ReadLine()
    let success, number = Int32.TryParse input
    if success then number else inputNumber ()
