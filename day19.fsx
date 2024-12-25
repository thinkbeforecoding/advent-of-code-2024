// --- Day 19: Linen Layout ---
// https://adventofcode.com/2024/day/19

open System
let lines = IO.File.ReadAllLines("input/day19.txt")
let towels = lines[0].Split(", ")
let designs = lines[2..]


// this first version is optimized to cut early, it used a stack
// to stay tail recursive
let rec findTowels' (design: string) stack impossible =
    match stack with
    | [] -> false       // all has been tried, and no combination was found
    | (i,n) :: rest ->
        if i = design.Length then
            // we reached the end of the design string, this is a solution
            true
        elif n = towels.Length then
            // we're out of towels to try at current index, there is no solution starting from i
            findTowels' design rest (Set.add i impossible)
        elif Set.contains i impossible then
            // we already found that there is no solution starting from i, don't try further
            findTowels' design rest impossible
        else
            let t = towels[n]
            // we may need to test next towel
            let stack = (i, n+1) :: rest
            // if current towel matches, put it on top of stack to try next
            let stack =
                if design.AsSpan(i: int).StartsWith(t) then
                    (i+t.Length, 0) :: stack
                else
                    stack
            findTowels' design stack impossible

let findTowels design = findTowels' design [(0,0)] Set.empty

findTowels designs[2]

designs |> Array.filter findTowels |> Array.length

// Part 2 requires to count everything, it was simpler with non tail recursive function
open System.Collections.Generic

// suffixes contains the lengths of all possible suffices starting at i
let rec findTowels2' (design: string) i n (suffixes: Dictionary<int,int64>) =
    // check if towel n match at current position
    let towel = towels[n]
    if design.AsSpan().Slice(i).StartsWith(towel) then
        // index after current towel
        let nexti = i+towel.Length
    
        if nexti = design.Length then
            // we reached the end, we have 1 solution from here
            1L
        else
            // look if we already the number of solutions from here
            match suffixes.TryGetValue(nexti) with
            | true, c -> c  // yes, return it
            | _  ->
                // no, compute it:
                // we sum all solution from next index and store it in suffixes
                let mutable count = 0L
                for n in 0 .. towels.Length-1 do
                    count <- count + findTowels2' design nexti n suffixes
                suffixes.Add(nexti, count)
                count
    else
        // no solution from here
        0L

let findTowels2 design = 
    let suffixes = Dictionary<int,int64>()
    let mutable count = 0L
    for n in 0 .. towels.Length-1 do
        count <- count + findTowels2' design 0 n suffixes
    count    

findTowels2 designs[0]
findTowels2 designs[2]
#time
designs |> Array.sumBy findTowels2 
