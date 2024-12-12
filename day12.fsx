open System

let input =
    IO.File.ReadAllText("input/day12.txt").ReplaceLineEndings("\n")

let w = input.IndexOf '\n'
let lw = w+1
let h = input.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

let toPos index = index % lw, index / lw
let toIndex (x,y) = x + y * lw

let inBounds (x,y) = x >= 0 && x < w && y >= 0 && y < h

let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2
let N = 0,-1
let E = 1, 0
let S = 0,1
let W = -1,0

let rotR (x,y) = -y,x
let rotL (x,y) = y,-x

let inv (x,y) = -x,-y

let inline get p =
   if inBounds p then 
        input[toIndex p]
   else
        '.'

/// A map with deduplicated chars
let map  = Array.replicate  (lw*h) '\000'

let mget  p = 
    if inBounds p then
        map[toIndex p]
    else
        '\000'
let mset p v = map[toIndex p] <- v

// place line ends (just for print)
for y in 0 .. h-1 do
    mset(w,y) '\n' 


let pushIf b h t = if b then h :: t else t

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
                |> pushIf (c = get (p++N)) (p++N)
                |> pushIf (c = get (p++E)) (p++E) 
                |> pushIf (c = get (p++S)) (p++S) 
                |> pushIf (c = get (p++W)) (p++W) 
                
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
    |> Seq.filter ((<>) '\n')
    |> Seq.countBy id
    |> Map.ofSeq


    
open System.Collections.Generic
// all perimeters lengths by key

// increment perimeter for key
let addIf cond c perimeters = 
    if cond then
        let v = 
            match Map.tryFind c perimeters with
            | Some v -> v+1
            | None -> 1
        Map.add c v perimeters
    else
        perimeters

// for each cell, look neigboors and increment perimeter in each direction if not same area
let perimeters= 
    let positions = seq {
        for y in 0 ..h-1 do
            for x in 0..h-1 do
                x,y }
    (Map.empty, positions)
    ||> Seq.fold (fun perimeters p ->
            let c = mget p
            perimeters
            |> addIf (c <> mget(p++N)) c 
            |> addIf (c <> mget(p++E)) c
            |> addIf (c <> mget(p++S)) c
            |> addIf (c <> mget(p++W)) c
    )


String map |> printfn "%s"

// Part 1
Map.keys perimeters
|> Seq.sumBy(fun k -> areas[k] * perimeters[k])

// find the external sides of area filled with key c, starting from start
// remove neighbors from set as we find them on the left
// as we find first occurence of key to start, it's alwas in a top left border, so
// we can search starting east, and finish when reaching start with direction north
let rec extSides c start p dir borders neighbors =
    if p = start && dir = N then
        // finished, just remove left cell from neighbors
        let l = rotL dir
        borders, Set.remove (mget (p++l)) neighbors
    else
        let l = rotL dir
        let r = rotR dir
        // remove left cell from neighbors
        let lc = mget (p++l)
        let neighbors =  Set.remove lc neighbors

        if c = lc then
            // left cell is in area, turn left and move to this cell
            // were changing border
            extSides c start (p++l) l (borders+1) neighbors
        elif c = mget (p++dir) then
            // next cell is area, continue straight, stay on same border
            extSides c start (p++dir) dir borders  neighbors
        else 
            // we have to turn right, this is a new border
            extSides c start p r (borders+1) neighbors


// find inner sides of area of key c
// same thing as before we start from top left
let rec innerSides c start p dir borders neighbors =
    if p = start && dir = N then
        // 
        borders, neighbors
    else
        let l = rotL dir
        let r = rotR dir
        // remove current cell from neighbors (there can be 2 adjacents area inside an outside area)
        let neighbors = Set.remove (mget p) neighbors
        if c <> mget (p++l)  then
            // the area continues on the left, new border
            innerSides c start (p++l) l (borders+1) neighbors
        elif c <> mget (p++dir) && mget(p++dir) <> '\000' then
            // the border is straight 
            innerSides c start (p++dir) dir borders neighbors
        else 
            // we need to turn right, new border
            innerSides c start p r (borders+1) neighbors

// find all neighbors for a given area
let neighbors c =
    let neighbors = Set.empty
    let positions = seq {
        for y in 0 .. h do
            for x in 0 ..w do 
                x,y  }
    (neighbors, positions)
    ||> Seq.fold (fun neighbors  p   ->
            if mget p = c then
                neighbors
                |> Set.add (mget (p++N))
                |> Set.add (mget (p++E))
                |> Set.add (mget (p++S)) 
                |> Set.add (mget (p++W))
            else
                neighbors)
    |> Set.remove('\000') 
    |> Set.remove(c)

// count total in side for area k (find unexplored holes in neighbors)
let rec inSides k neighbors  total =
    if Set.isEmpty neighbors then
        // all inner neighbors area have been explored 
        total
    else
        // next inner neighbor to explor
        let c = neighbors |> Seq.head 
        // top left point
        let p = map |> Array.findIndex((=)c) |> toPos 
        // find sides of hole, remove neighbors while explored
        let s, neighbors = innerSides k p p E 1 neighbors
        // try find remaining holes
        inSides k neighbors (total+s)
        

// compute the total number of sides
// start finding all neighbors,
// then count external sides, removing external neighbors
// find holes sides using remaining internal neighbors
let sides c = 
    let neighbors = neighbors c
    let i = map |> Array.findIndex((=) c)
    let p = toPos i
    let s, neighbors = extSides c p p E 1 neighbors
    let is = inSides c neighbors 0
    s + is

String map

// Part 2
areas.Keys
|> Seq.sumBy (fun c ->
    let s = sides c
    s * areas[c])
