﻿module IntcodeComputer.IntcodeComputer

open System
open Utils
open Utils

type Memory = {memory: int64 array; relativeBase: int64}

type ParameterMode = | Position | Immediate | Relative
module ParameterMode =
    let fromInt = function
        | 0 -> Position
        | 1 -> Immediate
        | 2 -> Relative
        | x -> raise (new ArgumentException(sprintf "Parameter mode '%d' undefined" x))
    
    let getValue (memory: Memory) parameter = function
        | Position -> memory.memory.[int parameter]
        | Immediate -> parameter
        | Relative -> memory.memory.[int <| parameter + memory.relativeBase]
    
    let address relativeBase value = function
        | Position -> value
        | Relative -> value + relativeBase
        | x -> raise (new ArgumentException(sprintf "Mode x can not be used to calculate an address: %A" x))

type Operation =
    | Halt
    | Add
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | AdjustRelativeBase
module Operation =
    let fromInt = function
        | 1 -> Add
        | 2 -> Multiply
        | 3 -> Input
        | 4 -> Output
        | 5 -> JumpIfTrue
        | 6 -> JumpIfFalse
        | 7 -> LessThan
        | 8 -> Equals
        | 9 -> AdjustRelativeBase
        | 99 -> Halt
        | x -> raise (new ArgumentException(sprintf "opcode '%d' undefined" x))

    let parameters = function
        | Halt -> 0
        | Add -> 3
        | Multiply -> 3
        | Input -> 1
        | Output -> 1
        | JumpIfTrue -> 2
        | JumpIfFalse -> 2
        | LessThan -> 3
        | Equals -> 3
        | AdjustRelativeBase -> 1
    
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
    let execute ``in`` out (memory: Memory) pos = function
        | Instruction(Halt, _) -> pos, memory.relativeBase
        | Instruction(Add, [mode1; mode2; mode3]) ->
            memory.memory.[int <| ParameterMode.address memory.relativeBase memory.memory.[pos+3] mode3] <-
                ParameterMode.getValue memory memory.memory.[pos+1] mode1 +
                ParameterMode.getValue memory memory.memory.[pos+2] mode2
            pos + 4, memory.relativeBase
        | Instruction(Multiply, [mode1; mode2; mode3]) ->
            memory.memory.[int <| ParameterMode.address memory.relativeBase memory.memory.[pos+3] mode3] <-
                ParameterMode.getValue memory memory.memory.[pos+1] mode1 *
                ParameterMode.getValue memory memory.memory.[pos+2] mode2
            pos + 4, memory.relativeBase
        | Instruction(Input, [mode]) ->
            memory.memory.[int <| ParameterMode.address memory.relativeBase memory.memory.[pos+1] mode] <- ``in`` ()
            pos + 2, memory.relativeBase
        | Instruction(Output, [mode]) ->
            out <| ParameterMode.getValue memory memory.memory.[pos+1] mode
            pos + 2, memory.relativeBase
        | Instruction(JumpIfTrue, [mode1; mode2]) ->
            if ParameterMode.getValue memory memory.memory.[pos+1] mode1 <> 0L
            then int <| ParameterMode.getValue memory memory.memory.[pos+2] mode2, memory.relativeBase
            else pos + 3, memory.relativeBase
        | Instruction(JumpIfFalse, [mode1; mode2]) ->
            if ParameterMode.getValue memory memory.memory.[pos+1] mode1 = 0L
            then int <| ParameterMode.getValue memory memory.memory.[pos+2] mode2, memory.relativeBase
            else pos + 3, memory.relativeBase
        | Instruction(LessThan, [mode1; mode2; mode3]) ->
            memory.memory.[int <| ParameterMode.address memory.relativeBase memory.memory.[pos+3] mode3] <-
                if ParameterMode.getValue memory memory.memory.[pos+1] mode1 < ParameterMode.getValue memory memory.memory.[pos+2] mode2
                then 1L
                else 0L
            pos + 4, memory.relativeBase
        | Instruction(Equals, [mode1; mode2; mode3]) ->
            memory.memory.[int <| ParameterMode.address memory.relativeBase memory.memory.[pos+3] mode3] <-
                if ParameterMode.getValue memory memory.memory.[pos+1] mode1 = ParameterMode.getValue memory memory.memory.[pos+2] mode2
                then 1L
                else 0L
            pos + 4, memory.relativeBase
        | Instruction(AdjustRelativeBase, [mode]) ->
            pos + 2, memory.relativeBase + ParameterMode.getValue memory memory.memory.[pos+1] mode
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


let rec runProgram inputLeft currentOutput memory pos =
    let mutable input = inputLeft
    let ``in`` () =
        if List.length input > 0
        then
            let num = input.[0]
            input <- input.[1..]
            num
        else
            Utils.inputNumber ()
    
    let mutable output = currentOutput
    let out num =
        output <- num :: output
    
    let newPos, newRelativeBase = Instruction.execute ``in`` out memory pos <| Instruction.fromInt (int memory.memory.[pos])
    
    if newPos = pos
    then List.rev output
    else runProgram input output {memory with relativeBase = newRelativeBase} newPos


let rec runUntilOutput inputLeft memory pos =
    let mutable input = inputLeft
    let ``in`` () =
        if List.length input > 0
        then
            let num = input.[0]
            input <- input.[1..]
            num
        else
            Utils.inputNumber ()
    
    let mutable output = None
    let out num =
        output <- Some num
    
    let newPos, newRelativeBase = Instruction.execute ``in`` out memory pos <| Instruction.fromInt (int memory.memory.[pos])
    
    match output with
    | None ->
        if newPos = pos
        then None
        else runUntilOutput input {memory with relativeBase = newRelativeBase} newPos
    | Some num -> Some (num, newPos, input)


let readInMemory file =
    (readFileContent file)
            .Split [| ',' |]
        |> Array.map int64


let newMemory (memory: int64 array) = {memory = Array.append (Array.copy memory) (Array.init 1000 (fun _ -> 0L)); relativeBase = 0L}
