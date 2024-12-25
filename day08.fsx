open System
// read the map, once again as a flat string
let map = IO.File.ReadAllText("input/day08.txt").ReplaceLineEndings("\n")

// the with of the map
let w = map.IndexOf('\n')
// the distance to move to next line
let lw = w+1
// the height of the map
let h = map.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

// get character at x,y
let get (x,y) = map[x + y * lw]

// check if x,y is in map
let isInMap (x,y) = x >= 0 && x < w && y >= 0 && y < h

// get a list of position for each antena char
// for this, make a pair of char position
// filter out all '.',
// group by char, and make a list of position
let positions =
    [ for y in 0..h-1 do
        for x in 0 .. w-1 do
            let p = x,y
            get p, p ]
    |> List.filter (fun (a,_) -> a <> '.')
    |> List.groupBy fst
    |> List.map (fun (a,l) -> a, l |> List.map snd)


// add vectors
let (++) (x1,y1) (x2,y2) = x1 + x2, y1+y2
// subtract verctors
let (--) (x1,y1) (x2,y2) = x1 - x2, y1 - y2
// invert vector
let inv (x,y) = -x,-y

// get antifreqencencies for given two antenas:
// take vector from one to other,
// go in this direction and opposit direction from both ends
// return them only if in map
let antiFreqs a1 a2= 
    let v = (a2 -- a1)
    let af1 = a1 -- v
    let af2 = a2 ++ v
    [ if isInMap af1 then
        af1
      if isInMap af2 then 
        af2 ]

// make all pairs (but only once for each) of elements of l
let allPairs l =
    [ for i in 0 .. List.length l-1 do
        for j in i+1 .. List.length l-1 do
            l[i],l[j]
    ]

// collect for each antena kind,
// the antifrequencies for all paires of antenas
// deduplicate positions, and count them 
positions
|> List.collect (fun (f , antenas) ->
    allPairs antenas
    |> List.collect (fun (a1, a2) ->
            antiFreqs a1 a2)
) |> List.distinct
|> List.length


// returns all positions using direction vector v, until outside of the map
let rec untilOut p v=
    [ 
        if isInMap p then
            p
            yield! untilOut (p++v) v
    ]

// returns all nodes for a pair of antenas
// go in both directions
let allNodes a1 a2 =
    let v = a2 -- a1
    [ yield!  untilOut a2 (inv v)
      yield! untilOut a1 v ]


// collects all nodes for all pairs of antenas
positions
|> List.collect (fun (f , antenas) ->
    allPairs antenas
    |> List.collect (fun (a1, a2) ->
            allNodes a1 a2)
) |> List.distinct
|> List.length