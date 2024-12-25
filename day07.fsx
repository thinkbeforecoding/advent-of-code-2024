// --- Day 7: Bridge Repair ---
// https://adventofcode.com/2024/day/7
open System

// convert each line as a pair of total and list of values
let input =
    IO.File.ReadAllLines("input/day07.txt")
    |> Array.map (fun l ->
        match l.Split(":") with
        | [|r;vs|] ->
            int64 r, (vs.Split(' ', StringSplitOptions.RemoveEmptyEntries)  |> Seq.map int64 |> Seq.toList)
        | _ -> failwith "Nope"
    )


// try to find operators recursively
// test is the expected value
// total is the current total of the evaluation
// values is the remaining values to add or multiply
let rec findOps test total values =
    match values with
    | [] ->
        // we used all values, check if the total matches the test value
        total = test
    | h :: rest ->
        // there is at least one value to add/mul
        if total > test then
            // the total is already greater than test
            // and we are not finished yet, and the total can only get bigger,
            // so no chance to find an answer in this direction, give up
            false
        else
            // start looking for solitions using * as the get to big very fast we can abandon quickly
            // for those that don't abandon, tests the +
            findOps test (total*h) rest
            || findOps test (total+h) rest

// check if the test can be validated from values
let isValid (test,values)= findOps test 0L values

// filter lines and sum them
input
|> Array.filter isValid
|> Array.sumBy fst


// Part 2
// count the number of digits of x (by dividing recursively by 10)
let rec digits' x d =
    if x = 0L then
        d
    else
        digits'(x/10L) (d+1) 
let digits x = digits' x 0

// Add d zeros to x
let rec addZ x d =
    if d = 0 then
        x
    else
        addZ (x*10L) (d-1)

// the || orperator, (called ||/ here to avoid conflict with boolean or)
let (||/) x y = 
     addZ x (digits y) + y


// similar to findOps, but with the new operation
let rec findOps2 test total values =
    match values with
    | [] ->
        total = test
    | h :: rest ->
        if total > test then
            false
        else
            findOps2 test (total*h) rest
            || findOps2 test (total||/h) rest
            || findOps2 test (total+h) rest

let isValid2 (test,values)= findOps2 test 0L values

input
|> Array.filter isValid2
|> Array.sumBy fst