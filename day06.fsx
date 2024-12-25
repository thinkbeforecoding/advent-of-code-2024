// --- Day 6: Guard Gallivant ---
// https://adventofcode.com/2024/day/6

open System
// Read all text
// this is actually simpler than splitting by lines
// especially, finding a specific char c is map.IndexOf(c) |> toPos
let map =
    IO.File.ReadAllText("input/day06.txt").ReplaceLineEndings("\n")

// the width of the map 
let width = map.IndexOf("\n")
// the length to move to next line
let lineWidth = width+1
// the height of the map
let height = map.Split("\n", StringSplitOptions.RemoveEmptyEntries).Length

// convert a index in string to (x,y) 
let toPos i =
    i%lineWidth, i/lineWidth

// convert (x,y) to index in string
let index (x,y) = y*lineWidth + x

// check if x,y is in the map area
let inArea(x,y) = x >= 0 && x < width && y >= 0 && y < height

// check if there is an obstacle at (x,y)
let obstacle p = map[index p] = '#'

// adds two vectors
let (++) (x1,y1) (x2,y2) = x1+x2, y1+y2
// rotate vector 90° clockwise
let rotRight (x,y) = (-y, x)
// find the initial position
let initPos = map.IndexOf('^') |> toPos
// the initial direction (north)
let north = (0,-1)


// Part 1
// Keep track of visited positions in a set, this way, it automatically deduplicates
// when we go several times through the same position
let rec path pos dir visited =
    // add current position to visited
    let visited = Set.add pos visited
    // next position
    let next = pos ++ dir
    if inArea next then
        // the position is in area, continue
        if obstacle next then
            // there is a position there, turn right
            let dir = rotRight dir
            // continue in this new direction
            path (pos ++ dir) dir visited
        else
            // continue forward
            path next dir visited
    else
        // we leave the area, count visited positions
        Set.count visited

// start from initial position, heading north
path initPos north Set.empty





// Part 2
// Keep track of turns in a set, the second time we make the same turn, it's a loop
// obst is the tested position of the obstacle 
// pos and dir are the current position and directions
// turns keeps track of positions and directions when
let rec makeLoop obst pos dir turns =
    // the next position
    let next = pos ++ dir
    // check if it is in the map area
    if inArea next then
        // check if next position is an obstacle from map,
        // or the specific obstacle we test
        if obstacle next || next = obst then
            // yes, check if we already turned here from the same direction
            if Set.contains (pos,dir) turns then
                // yes, we are in a loop
                true
            else
                // no, add current position and direction to set, so we can see later if we make the same turn
                let turns = Set.add (pos,dir) turns
                // turn right and continue
                let dir = rotRight dir
                makeLoop obst pos dir turns
        else
            makeLoop obst next dir turns
    else
        // not in the area, the obstacle doesn't create a loop
        false

// Generate all possible obstacle positions (where there no existing obstacle or init position)
// and check if it makes a loop
// count them
seq { 
    for y in 0 .. height-1 do
        for x in 0 .. width-1 do
            if not(obstacle (x,y) || (x,y) = initPos) then
                x,y
 } |> Seq.filter (fun obst -> makeLoop obst initPos north Set.empty)
|> Seq.length


