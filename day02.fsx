open System

let input =
    IO.File.ReadAllLines("input/day02.txt")
    |> Array.map (fun l ->
        l.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int 
    )

let pos diffs =
    diffs |> Array.forall (fun l -> l > 0 && l <= 3)
let neg diffs =
    diffs |> Array.forall (fun l -> l < 0 && l >= -3)

let safe report= 
    let diffs = 
        report
        |> Array.pairwise
        |> Array.map (fun (x,y) -> y-x)
    pos diffs || neg diffs

input
|> Array.filter safe
|> Array.length


// Part 2

let dampenerSafe report =
    if safe report then
        true
    else
        [0 .. Array.length report - 1] 
        |> List.exists (fun i ->
            report
            |> Array.removeAt i
            |> safe)

input
|> Array.filter dampenerSafe
|> Array.length
