module Utils.Utils

open System.IO

let flip f y x = f x y

let readFile path =
    File.ReadLines path
