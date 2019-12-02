module Utils.Utils

open System.IO

let flip f y x = f x y

let readFileLines =
    File.ReadLines

let readFileContent =
    File.ReadAllText
