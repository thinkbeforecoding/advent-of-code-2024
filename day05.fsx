open System
// all input lines
let input = 
    IO.File.ReadAllLines("input/day05.txt")
    
// take first part, and convert each line to a pair of int
let rules =
    input
    |> Array.takeWhile (String.IsNullOrWhiteSpace>>not)
    |> Array.map (fun l -> 
        match l.Split('|') with
        | [| left;right|] -> int left, int right
        | _ -> failwith "Nope")

// take the second part and convert each line to a int[]
let updates =
    input
    |> Array.skipWhile(String.IsNullOrEmpty>>not)
    |> Array.skip 1
    |> Array.map (fun l -> 
        l.Split(',')
        |> Array.map int)

// validate a rule for an update
// the if pages rleft and rright are both in the update,
// the rleft should come before rright
let validate update (rleft,rright) =
    let ileft = Array.tryFindIndex (fun p -> p= rleft) update
    let iright = Array.tryFindIndex (fun p -> p=rright) update
    match ileft, iright with
    | Some il, Some ir -> il < ir
    | _ -> true

// find filter updates where all rules apply
// get the page in the middle and sum them
updates
|> Array.filter (fun update ->
    rules |> Array.forall (validate update)
)
|> Array.map (fun update -> update[ update.Length/2])
|> Array.sum


// Part 2

// cmp is a custom comparer for pages p1 and p2,
// it returns -1 if x comes before y, 1 otherwise
// there should be no equality to it never returns 0
// it arbitraryly return 1 if we find no rule

let compare p1 p2 =
    match rules |> Array.tryFind(fun (x,y) -> (x=p1 && y=p2) || (x=p2 && y=p1)) with
    | Some(x,y) ->
        if p1 = x then
            -1
        else
            1
    | None -> 1




// Keep updates that do not validate rules
// and sort them using the custom comparer
// then sum values in the middle
updates
|> Array.filter (fun update ->
    rules |> Array.exists (fun u -> not(validate update u))
)
|> Array.map(Array.sortWith compare)
|> Array.map (fun update -> update[ update.Length/2])
|> Array.sum 