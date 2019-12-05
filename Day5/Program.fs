open System
open Utils.Utils

type Memory = int array

type ParameterMode = | Position | Immediate
module ParameterMode =
    let fromInt = function
        | 0 -> Position
        | 1 -> Immediate
        | x -> raise (new ArgumentException(sprintf "Parameter mode '%d' undefined" x))
    
    let getValue (memory: Memory) parameter = function
        | Position -> memory.[parameter]
        | Immediate -> parameter

type Operation =
    | Halt
    | Add
    | Multiply
    | Input
    | Output
module Operation =
    let fromInt = function
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> Input
        | 4 -> Output
        | 99 -> Halt
        | x -> raise (new ArgumentException(sprintf "opcode '%d' undefined" x))

    let parameters = function
        | Halt -> 0
        | Add -> 3
        | Multiply -> 3
        | Input -> 1
        | Output -> 1
    
    let padArguments instruction arguments =
        let providedArguments = List.length arguments
        let neededArguments = parameters instruction
        
        if providedArguments = neededArguments then
            arguments
        elif providedArguments > neededArguments then
            List.take neededArguments arguments
        else
            arguments @ List.replicate (neededArguments - providedArguments) Position

type Instruction = Instruction of operation: Operation * parameterModes: ParameterMode list
module Instruction =
    let execute (memory: Memory) pos = function
        | Instruction(Halt, _) -> pos
        | Instruction(Add, [mode1; mode2; _]) ->
            memory.[memory.[pos+3]] <-
                ParameterMode.getValue memory memory.[pos+1] mode1 +
                ParameterMode.getValue memory memory.[pos+2] mode2
            pos + 4
        | Instruction(Multiply, [mode1; mode2; _]) ->
            memory.[memory.[pos+3]] <-
                ParameterMode.getValue memory memory.[pos+1] mode1 *
                ParameterMode.getValue memory memory.[pos+2] mode2
            pos + 4
        | Instruction(Input, _) ->
            memory.[memory.[pos+1]] <- 1
            pos + 2
        | Instruction(Output, [mode]) ->
            printfn "%d" <| ParameterMode.getValue memory memory.[pos+1] mode
            pos + 2
        | instruction ->
            raise (new ArgumentException(sprintf "Illegal instruction: %A" instruction))

    let fromInt instruction =
        if instruction < 10
        then
            let operation = Operation.fromInt instruction
            let arguments = Operation.padArguments operation []
            Instruction(operation, arguments)
        else
            let instructionString = string instruction
            let startOpcode = instructionString.Length - 2
            let operation = Operation.fromInt (int (instructionString.Substring(startOpcode)))
            let arguments = instructionString.Substring(0, startOpcode)
                            |> Seq.map (fun c -> int c - int '0')
                            |> Seq.map ParameterMode.fromInt
                            |> Seq.rev
                            |> Seq.toList
                            |> Operation.padArguments operation
            
            Instruction(operation, arguments)


let rec executeProgram memory pos =
    let newPos = Instruction.execute memory pos <| Instruction.fromInt memory.[pos]
    
    if newPos = pos then
        ()
    else
        executeProgram memory newPos


[<EntryPoint>]
let main _ = 
    let memory =
        (readFileContent "input1.txt")
            .Split [| ',' |]
        |> Array.map int
    
    executeProgram memory 0
    
    0 // return an integer exit code
