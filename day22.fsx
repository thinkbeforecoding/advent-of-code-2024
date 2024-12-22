open System

let secrets =
    IO.File.ReadAllLines("input/day22.txt")
    |> Array.map int

let inline prune x = x &&& 0xFFFFFF

let next s =
    let s1 = ((s <<< 6) ^^^ s) |> prune
    let s2 = ((s1 >>> 5) ^^^ s1) |> prune
    ((s2 <<< 11) ^^^ s2) |> prune


let rec secretAt n s =
    if n = 0 then
        s
    else
        secretAt (n-1) (next s)

secrets
|> Array.sumBy (secretAt 2000 >> int64)


let rec prices s =
    seq {
        yield  s % 10
        yield! prices (next s)
    }

let diffs (p: int seq) = 
    p |> Seq.pairwise 
    |> Seq.map (fun (x,y) -> y-x, y) 
    |> Seq.windowed 4
    |> Seq.map (function [| (a,_);(b,_);(c,_);(d,p) |] -> (a,b,c,d), p )


prices 123 |> Seq.pairwise |> Seq.map (fun (x,y) -> y-x)


let allPrices = secrets |> Array.map (fun s ->  prices s |> Seq.take 2000 |> diffs |> Seq.toList) 

let allSeqs = allPrices |> Seq.collect  id|> Seq.map fst |> set |> Set.toArray

let totalPrice sq =
    allPrices |> Array.sumBy (fun  ps -> ps |> List.tryFind(fun (s,_) -> s = sq) |> Option.map snd |>  Option.defaultValue 0)

let bestSequence =
     allSeqs |> Array.Parallel.maxBy (fun s -> totalPrice s)

totalPrice bestSequence