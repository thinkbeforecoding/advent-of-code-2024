// --- Day 11: Plutonian Pebbles ---
// https://adventofcode.com/2024/day/11

open System
let input =
    IO.File.ReadAllText("input/day11.txt").Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map(fun s -> int64 s, 1L)

/// counts the number of digits in n
let rec digits' n d =
    if n < 10L then
        d+1
    else
        digits' (n/10L) (d+1)

let digits n = digits' n 0


// returns next stones for one stone
// will be used with Array.collect
let next (n, c) =
    if n = 0L then
        [|1L,c|]
    else
        let d = digits n
        if d&&&1 = 0 then // check whether even
            let split = pown 10L (d/2) // find where to split
            let struct(left, right) = Int64.DivRem(n,split)
            [| left,c; right,c |]
        else
            [| n * 2024L, c |]

// The original version was just using Array.collect
// This was working fine for 25 blinking
// But after 35 it was far too slow...
// The trick is to notice that a many stones have the same number on it
// and they will evolve exactly in the same way.
// Instead of evolving each stone individually, they can all be evolved in one step
// if we keep track of the count.
// After each pass, stones with the same number are grouped and counted 
let rec loop n (stones: (int64 * int64)[]) =
    if n = 0 then
        stones |> Array.sumBy snd
    else
        let stones = 
            Array.collect next stones
            |> Seq.groupBy fst
            |> Seq.map (fun (n, st) -> n, Seq.sumBy snd st)
            |> Seq.toArray


        loop (n-1) stones

loop 25 input
loop 75 input