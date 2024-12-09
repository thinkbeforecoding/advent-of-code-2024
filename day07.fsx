open System

let input =
    IO.File.ReadAllLines("input/day07.txt")
    |> Array.map (fun l ->
        match l.Split(":") with
        | [|r;vs|] ->
            int64 r, (vs.Split(' ', StringSplitOptions.RemoveEmptyEntries)  |> Seq.map int64 |> Seq.toList)
        | _ -> failwith "Nope"
    )

let test, values = input[0]

let rec findOps test total values =
    match values with
    | [] ->
        total = test
    | h :: rest ->
        if total > test then
            false
        else
            findOps test (total*h) rest
            || findOps test (total+h) rest

let isValid (test,values)= findOps test 0L values

input
|> Array.filter isValid
|> Array.sumBy fst

let rec digits' x d =
    if x = 0L then
        d
    else
        digits'(x/10L) (d+1) 
let digits x = digits' x 0

let rec addZ x d =
    if d = 0 then
        x
    else
        addZ (x*10L) (d-1)

let (||/) x y = 
     addZ x (digits y) + y


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