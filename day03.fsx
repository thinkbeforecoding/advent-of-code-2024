#r "nuget: FSharp.Text.RegexProvider"
open System
open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

// today, we use regex to parse

let input = IO.File.ReadAllText("input/day03.txt")
// a regex to extract the arguments from uncorrupted mul instructions
type Rx = Regex< @"mul\((?<l>\d{1,3}),(?<r>\d{1,3})\)" >

// Find all matches, read inputs as int, and multiply them
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

// a regex to find do,don't and mul.
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
// enabled switch to true with do(), to false with don't(). controls whether we add the results
// total is the running total
// tokens is the list of remaining instructions
let rec eval' enabled total tokens =
    match tokens with
    | [] ->
        // we reached the end of the tokens, total contains the result
        total
    | Do :: rest ->
        // we found a do() instruction,  continue in enabled mode
        eval'  true total rest
    | Dont :: rest ->
        // we found a don't() instruction, continue in disabled mode
        eval'  false total rest
    | Mul(l,r) :: rest ->
        // we found a mul(l,r) instruction,
        // add it to total only if enabled
        if enabled then
            eval' enabled (total + l*r) rest
        else
            eval' enabled total rest

// start in enabled mode with a total of 0
let eval  = eval' true 0

// evaluate the full input
input
|> parse
|> eval