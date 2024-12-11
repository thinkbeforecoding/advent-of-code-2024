open System

// let map = 
//     """............
// ........0...
// .....0......
// .......0....
// ....0.......
// ......A.....
// ............
// ............
// ........A...
// .........A..
// ............
// ............"""
let map = IO.File.ReadAllText("input/day08.txt").ReplaceLineEndings("\n")

let w = map.IndexOf('\n')
let lw = w+1
let h = map.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

let get (x,y) = map[x + y * lw]
let isInMap (x,y) = x >= 0 && x < w && y >= 0 && y < h

let positions =
    [ for y in 0..h-1 do
        for x in 0 .. w-1 do
            let p = x,y
            get p, p ]
    |> List.filter (fun (a,_) -> a <> '.')
    |> List.groupBy fst
    |> List.map (fun (a,l) -> a, l |> List.map snd)


let (++) (x1,y1) (x2,y2) = x1 + x2, y1+y2
let (--) (x1,y1) (x2,y2) = x1 - x2, y1 - y2
let (%%) (x,y) n = x/2,y/2 
let inv (x,y) = -x,-y

let antiFreqs a1 a2= 
    let v = (a2 -- a1)
    let af1 = a1 -- v
    let af2 = a2 ++ v
    [ if isInMap af1 then
        af1
      if isInMap af2 then 
        af2 ]

let allPairs l =
    [ for i in 0 .. List.length l-1 do
        for j in i+1 .. List.length l-1 do
            l[i],l[j]
    ]

positions
|> List.collect (fun (f , antenas) ->
    allPairs antenas
    |> List.collect (fun (a1, a2) ->
            antiFreqs a1 a2)
) |> List.distinct
|> List.length


let rec untilOut p v=
    [ 
        if isInMap p then
            p
            yield! untilOut (p++v) v
    ]

let allNodes a1 a2 =
    let v = a2 -- a1
    [ yield!  untilOut a2 (inv v)
      yield! untilOut a1 v ]


positions
|> List.collect (fun (f , antenas) ->
    allPairs antenas
    |> List.collect (fun (a1, a2) ->
            allNodes a1 a2)
) |> List.distinct
|> List.length