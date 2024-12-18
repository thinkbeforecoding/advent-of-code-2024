open System
let input = 
    IO.File.ReadAllLines("input/day05.txt")
    
let rules =
    input
    |> Array.takeWhile (String.IsNullOrWhiteSpace>>not)
    |> Array.map (fun l -> 
        match l.Split('|') with
        | [| left;right|] -> int left, int right
        | _ -> failwith "Nope")

let updates =
    input
    |> Array.skipWhile(String.IsNullOrEmpty>>not)
    |> Array.skip(1)
    |> Array.map (fun l -> 
        l.Split(',')
        |> Array.map int)

let validate update (rleft,rright) =
    let ileft = Array.tryFindIndex (fun p -> p= rleft) update
    let iright = Array.tryFindIndex (fun p -> p=rright) update
    match ileft, iright with
    | Some il, Some ir -> il < ir
    | _ -> true

updates
|> Array.filter (fun update ->
    rules |> Array.forall (validate update)
)
|> Array.map (fun update -> update[ update.Length/2])
|> Array.sum



let cmp p1 p2 =
    match rules |> Array.tryFind(fun (x,y) -> (x=p1 && y=p2) || (x=p2 && y=p1)) with
    | Some(x,y) ->
        if p1 = x then
            -1
        else
            1
    | None -> 1





updates
|> Array.filter (fun update ->
    rules |> Array.exists (fun u -> not(validate update u))
)
|> Array.map(Array.sortWith cmp)
|> Array.map (fun update -> update[ update.Length/2])
|> Array.sum 