#r "nuget: FSharp.Text.RegexProvider"
open System
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

// today, we use regex to parse

let input = IO.File.ReadAllText("input/day03.txt")
type Rx = Regex< @"mul\((?<l>\d{1,3}),(?<r>\d{1,3})\)" >

Rx().TypedMatches(input)
|> Seq.sumBy (fun m ->
    let l = int m.l.AsInt
    let r = int m.r.AsInt
    l*r
)

// part 2

// model instructions
type Instruction =
    | Mul of int * int
    | Do
    | Dont

type Rx2 = Regex< @"(?<do>do\(\))|(?<dont>don't\(\))|mul\((?<l>\d{1,3}),(?<r>\d{1,3})\)" >

// parse all instructions in order
let parse input =
    Rx2().TypedMatches(input)
    |> Seq.map (fun m ->
        if m.dont.Success then
            Dont
        elif m.``do``.Success then
            Do
        else
            let l = int m.l.AsInt
            let r = int m.r.AsInt
            Mul(l,r)
    )
    |> Seq.toList

// eval total sum keeping track of last do/don't
let rec eval' enabled sum tokens =
    match tokens with
    | [] -> sum
    | Do :: rest ->
        eval'  true sum rest
    | Dont :: rest ->
        eval'  false sum rest
    | Mul(l,r) :: rest ->
        if enabled then
            eval' enabled (sum + l*r) rest
        else
            eval' enabled sum rest
let eval  = eval' true 0

input
|> parse
|> eval