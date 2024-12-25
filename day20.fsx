// --- Day 20: Race Condition ---
// https://adventofcode.com/2024/day/20

open System

let input =
    IO.File.ReadAllText("input/day20.txt").ReplaceLineEndings("\n")

// width of map
let w = input.IndexOf('\n')
let lw = w+1

// add vectors
let (++) (x,y) (x',y') = x+x', y+y'

// rotate right
let rotR (x,y )= -y,x
// rotate left
let rotL (x,y) = y,-x
// multiply by n
let ( ** ) (x,y) n = x*n,y*n
// convert index in string to x,y
let toPos index = index%lw, index/lw
// convert x,y to index in string
let toIndex (x,y) = x+y*lw

// get rid of E (to simplify code)
let map  = input.Replace('E', '.')

// get char at x,y
let get p = map[toIndex p]
// the start position
let start = input.IndexOf("S") |> toPos
// the end position
let end' = input.IndexOf("E") |> toPos


// the 4 main directions
let N = (0,-1)
let E = rotR N
let S = rotR E
let W = rotR S

// the start direction (the only one with '.')
let startDir =
    if get (start ++ N) = '.' then N
    elif get (start ++ E) = '.' then E
    elif get (start ++ S) = '.' then S
    else W


// follow the track and  direction and distance from start for each point 
let rec followTrack pos dir distance track =

    let track = Map.add pos (dir,distance) track
    if pos = end' then
        // we reached the end
        track
    else
        // first try ahead
        let ahead = pos++dir
        if get ahead = '.' then
            followTrack ahead dir (distance+1) track
        else
            // this is blocked, try right
            let right = rotR dir
            let posRight = pos++right
            if get posRight = '.' then
                followTrack posRight right (distance+1) track
            else
                // finally go left
                let left = rotL dir
                let posLeft = pos ++ left
                followTrack posLeft left (distance+1) track

// Get the full track
let track = followTrack start startDir 0 Map.empty


// check if there is a cheat in direction (rotated with rot)
let cheat rot pos =
    // get the direction and distance at position
    let dir,distance = Map.find pos track
    // turn direction
    let dir' = rot dir

    // next point is 2 moves in dir
    let next = pos ++ dir' ** 2
    // try find distance there (and remove the 2ps of the cheat itself)
    match Map.tryFind  next track with
    | Some (_,l2) -> l2 - distance - 2 
    | None -> 0


// cheats in each direction greater than 100ps
let cheatsLeft = track.Keys |> Seq.filter (fun p -> cheat rotL p >= 100 )  |> Seq.length
let cheatRigth = track.Keys |> Seq.filter (fun p -> cheat rotR p >= 100 ) |> Seq.length
let cheatForward = track.Keys |> Seq.filter (fun p -> cheat id p >= 100 ) |> Seq.length
let cheatBack = track.Keys |> Seq.filter (fun p -> cheat (rotL >> rotL) p >= 100 ) |> Seq.length
    
// all cheats
cheatsLeft + cheatRigth + cheatForward + cheatBack

// The manhattan distance
let manhattan (x,y) (x',y') = abs (x'-x) + abs(y'-y)

// take all pairs of points in track (it's ok, the track is not that long)
// the cheat length is given by the manhattan distance, it must be <= 20ps
// then the cheat gain is is the distance difference - cheat length and must be >= 100ps
let allPairs =
    [
    for p1, (_, d1) in track |> Map.toSeq do
        for p2, (_, d2) in track |> Map.toSeq  do
            let cheatLen = manhattan p1 p2
            if  cheatLen <= 20 &&  (d2 - d1) - cheatLen >= 100 then
                // there is a pottential cheat here
                p1,p2
    ]
allPairs.Length

