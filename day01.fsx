// --- Day 1: Historian Hysteria ---
// https://adventofcode.com/2024/day/1

open System
open System.IO

let input = File.ReadAllLines("input/day01.txt")

// read input as a (int * int)[]
let lines =
    input
    |> Array.map (fun l ->
        match l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) with
        | [| l;r|] -> int l, int r
        | _ -> failwith "Nope")

// unzip left and right columns
let left,right = Array.unzip lines

// compute distance
let dist x y = abs (x - y) 

// Sort both columns and compute distances, then sum
Array.map2 dist (Array.sort left) (Array.sort right)
|> Array.sum

//---- Part 2

// Small trick here,
// the idea is to find instances of x in right columns,
// then multiply by x, but instead of multiplying after,
// we can add x each time it's found.
 
left
|> Array.sumBy (fun x ->
    right
    |> Array.sumBy (fun y -> if x=y then x else 0) 
)