open System

// split text by double line ending
let input =IO.File.ReadAllText("input/day25.txt").ReplaceLineEndings("\n") .Split("\n\n")

// indicates if string is lock (should start with #)
let isLock (s: string) = s[0] = '#'

// compute height in column n
let height n (s:string) =
    let h = Seq.init 7 (fun y -> if s[n+y*6] = '#' then 1 else 0) |> Seq.sum
    h - 1

// computes heights for all coumns
let heights s = [| for n in 0 .. 4 do height n s |]

// apply a function on both parts of a pair
let both f (x,y) = f x, f y
// split input strings in locks and keys 

// get locks and keys appart
let locks, keys = 
    input 
    |> Array.partition isLock 
    |> both (Array.map heights)


// check if key and lock fit (all sums of heights should be <= 5)
let fit (key, lock) =
    (key,lock) ||> Seq.forall2 (fun k l -> k + l <= 5)

// take all combinations of keys and locks and check the y fit. count them
Seq.allPairs keys locks
|> Seq.filter fit
|> Seq.length