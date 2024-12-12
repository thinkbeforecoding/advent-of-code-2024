open System

let input =
    IO.File.ReadAllText("input/day12.txt").ReplaceLineEndings("\n")

let w = input.IndexOf '\n'
let lw = w+1
let h = input.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

let toPos index = index % lw, index / lw

let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2
let N = 0,-1
let E = 1, 0
let S = 0,1
let W = -1,0

let rotR (x,y) = -y,x
let rotL (x,y) = y,-x

let inv (x,y) = -x,-y

let inline get (x,y) =
   if x >= 0 && x < w && y >= 0 && y < h then 
        input[x + lw * y]
   else
        '.'

/// A map with deduplicated chars
let map  = Array.replicate  (lw*h) '\000'

let mget  (x,y) = 
    if x >= 0 && x < w && y >= 0 && y < h then
        map[x+y*lw]
    else
        '\000'
let mset (x,y) v = map[x+y*lw] <- v
for y in 0 .. h-1 do
    mset(w,y) '\n' 


let push b h t = if b then h :: t else t

// fills area containing c with value v (first position should be on stack)
let rec fill c v stack =
    match stack with
    | [] -> ()
    | p :: rest ->
        if mget p <> '\000' then 
            // outside of the map, nothing to do
            fill c v rest
        else
            // set value
            mset p v
            // push all neighbors on stack
            let stack =
                rest
                |> push (c = get (p++N)) (p++N)
                |> push (c = get (p++E)) (p++E) 
                |> push (c = get (p++S)) (p++S) 
                |> push (c = get (p++W)) (p++W) 
                
            fill c v stack

// fills all areas that have not been filled yet (still contain char 0)
let rec fillAll v =
    match Array.tryFindIndex ((=) '\000') map with
    | Some i ->
        let p = toPos i
        fill (get p) v [p] 
        // fill next area with next char
        fillAll (char(int v+1))
    | None -> v

// fill all starting at 'a'
fillAll 'a'

String map |> printfn "%s"

// count cells for each area, and keep them
let areas =
    map
    |> Array.filter (fun c -> c <> '\n')
    |> Array.countBy id
    |> dict


    
open System.Collections.Generic
// all perimeters lengths by key
let perimeters = Dictionary<char, int>()
// increment perimeter for key
let add c = 
    let v = 
        match perimeters.TryGetValue(c) with
        | true, v -> v+1
        | false,_ -> 1
    perimeters[c] <- v

// for each cell, look neigboors and increment perimeter in each direction if not same area
for y in 0 ..h-1 do
    for x in 0..h-1 do
        let p = x,y
        let c = mget (x,y)
        if c <> mget(p++N) then add c
        if c <> mget(p++E) then add c
        if c <> mget(p++S) then add c
        if c <> mget(p++W) then add c


String map |> printfn "%s"

// Part 1
perimeters.Keys
    |> Seq.sumBy(fun k -> areas[k] * perimeters[k])

// find the external sides of area filled with key c, starting from start
// remove neighbors from set as we find them on the left
// as we find first occurence of key to start, it's alwas in a top left border, so
// we can search starting east, and finish when reaching start with direction north
let rec extSides c start p dir borders (neis: HashSet<_>) =
    if p = start && dir = N then
        // finished, just remove left cell from neighbors
        let l = rotL dir
        neis.Remove(mget (p++l)) |> ignore
        borders
    else
        let l = rotL dir
        let r = rotR dir
        // remove left cell from neighbors
        let lc = mget (p++l)
        neis.Remove(lc) |> ignore

        if c = lc then
            // left cell is in area, turn left and move to this cell
            // were changing border
            extSides c start (p++l) l (borders+1) neis
        elif c = mget (p++dir) then
            // next cell is area, continue straight, stay on same border
            extSides c start (p++dir) dir borders  neis
        else 
            // we have to turn right, this is a new border
            extSides c start p r (borders+1) neis


// find inner sides of area of key c
// same thing as before we start from top left
let rec innerSides c start p dir borders (neis: HashSet<_>) =
    if p = start && dir = N then
        // 
        borders
    else
        let l = rotL dir
        let r = rotR dir
        // remove current cell from neighbors (there can be 2 adjacents area inside an outside area)
        neis.Remove(mget p) |> ignore
        if c <> mget (p++l)  then
            // the area continues on the left, new border
            innerSides c start (p++l) l (borders+1) neis
        elif c <> mget (p++dir) && mget(p++dir) <> '\000' then
            // the border is straight 
            innerSides c start (p++dir) dir borders neis
        else 
            // we need to turn right, new border
            innerSides c start p r (borders+1) neis

// find all neighbors for a given area
let neighbors c =
    let neis = HashSet()
    for y in 0 .. h do
        for x in 0 ..w do 
            let p = x,y
            if mget p = c then
                neis.Add(mget (p++N)) |> ignore
                neis.Add(mget (p++E)) |> ignore
                neis.Add(mget (p++S)) |> ignore
                neis.Add(mget (p++W)) |> ignore
    neis.Remove('\000') |> ignore
    neis.Remove(c) |> ignore
    neis

// count total in side for area k (find unexplored holes in neis)
let rec inSides k (neis: _ HashSet) total =
    if neis.Count = 0 then
        // all inner neighbors area have been explored 
        total
    else
        // next inner neighbor to explor
        let c = neis |> Seq.head 
        // top left point
        let p = map |> Array.findIndex((=)c) |> toPos 
        // find sides of hole, remove neighbors while explored
        let s = innerSides k p p E 1 neis
        // try find remaining holes
        inSides k neis (total+s)
        

// compute the total number of sides
// start finding all neighbors,
// then count external sides, removing external neighbors
// find holes sides using remaining internal neighbors
let sides c = 
    let neis = neighbors c
    let i = map |> Array.findIndex((=) c)
    let p = toPos i
    let s = extSides c p p E 1 neis
    let is = inSides c neis 0
    s + is

String map

// Part 2
areas.Keys
|> Seq.sumBy (fun c ->
    printfn "%c" c
    let s = sides c
    s * areas[c])
