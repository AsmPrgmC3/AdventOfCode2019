open Utils.Utils


let calculateFuel =
    flip (/) 3
    >> flip (-) 2


let rec calculateExtraFuel fuel =
    let extra = calculateFuel fuel
    if extra <= 0
        then 0
        else extra + calculateExtraFuel extra


let calculateTotalFuel mass =
    let fuel = calculateFuel mass
    let extra = calculateExtraFuel fuel
    fuel + extra


let part1 lines =
    lines
    |> Seq.map int
    |> Seq.map calculateFuel
    |> Seq.sum


let part2 lines =
    lines
    |> Seq.map int
    |> Seq.map calculateTotalFuel
    |> Seq.sum


[<EntryPoint>]
let main _ = 
    let lines = readFileLines "input1.txt"
    
    printfn "Required Fuel by modules: %d" <| part1 lines
    printfn "Total required Fuel: %d" <| part2 lines
    
    0 // return an integer exit code
