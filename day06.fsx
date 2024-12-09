open System
let map =
    IO.File.ReadAllText("input/day06.txt").ReplaceLineEndings("\n")
// let map =
//     """....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#..."""

let width = map.IndexOf("\n")
let lineWidth = width+1
let height = map.Split("\n", StringSplitOptions.RemoveEmptyEntries).Length

let toPos i =
    i%lineWidth, i/lineWidth

let index (x,y) = y*lineWidth + x

let inArea(x,y) = x >= 0 && x < width && y >= 0 && y < height

let obstacle p = map[index p] = '#'

let (++) (x1,y1) (x2,y2) = x1+x2, y1+y2
let rotRight (x,y) = (-y, x)
let initPos = map.IndexOf('^') |> toPos
let north = (0,-1)


// Part 1
// Keep track of visited positions in a set
let rec path pos dir visited =
    let visited = Set.add pos visited
    let next = pos ++ dir
    if inArea next then
        if obstacle next then
            let dir = rotRight dir
            path (pos ++ dir) dir visited
        else
            path next dir visited
    else
        Set.count visited

path initPos north Set.empty





// Part 2
// Keep track of turns in a set, the second time we make the same turn, it's a loop
let rec makeLoop obst pos dir turns =
    let next = pos ++ dir
    if inArea next then
        if obstacle next || next = obst then
            if Set.contains (pos,dir) turns then
                true
            else
                let turns = Set.add (pos,dir) turns
                let dir = rotRight dir
                makeLoop obst pos dir turns
        else
            makeLoop obst next dir turns
    else
        false

[
    for y in 0 .. height-1 do
        for x in 0..width-1 do
            if not(obstacle (x,y) || (x,y) = initPos) then
                if makeLoop  (x,y) initPos north Set.empty then
                    x,y
] |> List.length

