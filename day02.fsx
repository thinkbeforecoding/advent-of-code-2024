open System

// parse input as array of int[]
let input =
    IO.File.ReadAllLines("input/day02.txt")
    |> Array.map (fun l ->
        l.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int 
    )

// check if all values are >0 and <=3
let pos diffs =
    diffs |> Array.forall (fun l -> l > 0 && l <= 3)
// check if all values are >=-3 and < 0 
let neg diffs =
    diffs |> Array.forall (fun l -> l < 0 && l >= -3)

// check if safe
let safe report= 
    // the differences between consecutives numbers
    let diffs = 
        report
        |> Array.pairwise
        |> Array.map (fun (x,y) -> y-x)
    // check if pos or neg applies
    pos diffs || neg diffs

// count reports that are sage
input
|> Array.filter safe
|> Array.length


// Part 2

// if the report is true, no problem,
// otherwhise, test if removing at any position can make it safe
let dampenerSafe report =
    if safe report then
        true
    else
        {0 .. Array.length report - 1} 
        |> Seq.exists (fun i ->
            report
            |> Array.removeAt i
            |> safe)

// count safe inputs
input
|> Array.filter dampenerSafe
|> Array.length
