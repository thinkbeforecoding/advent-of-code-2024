// --- Day 15: Warehouse Woes ---
// https://adventofcode.com/2024/day/15

open System

let parse (input: string) = 
    // let all = IO.File.ReadAllText("input/day15.txt").ReplaceLineEndings("\n")

    let split = input.IndexOf("\n\n")
    let map = input[0..split] |> Seq.toArray
    let moves = input[split..].Replace("\n","")
    map, moves


// add vectors
let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2

// convert from index to x,y given linewidth
let toPos lw index= index%lw, index/lw

// convert x,y to index given line width
let toIndex lw (x,y) = x+y*lw

// get char at x,y
let get lw  (map: char[]) p = map[toIndex lw p]
// set char at x,y
let set lw (map: char[]) p v = map[toIndex lw p] <- v

// convert move n to direction vector
let getDir (moves: string) i =
    match moves[i] with
    | '<' -> -1,0
    | '^' -> 0, -1
    | '>' -> 1,0
    | 'v' -> 0,1
    | _ -> failwith "Unknown move"

// find initial pos x,y
let initialPos lw map = Array.IndexOf(map, '@') |> toPos lw

// moves in direction as long as it's boxes, returns position after last box
let rec behindBoxes lw map p dir =
    let next = p++dir
    if get lw map next = 'O' then
        behindBoxes lw map next dir
    else
        next

// move iteration
let rec move lw (moves: string) map n pos =
    if n < moves.Length then
        // get direction
        let dir = getDir moves n
        // the next position (if we can move)
        let next = pos++dir
        // the actual next position
        let newPos = 
            match get lw map next with
            | '#' -> 
                // a wall, we can't move
                pos
            | 'O' ->
                // a box, look what's behind all boxes
                let behind = behindBoxes lw map next dir
                if get lw map behind = '#' then
                    // a wall, nothing moves
                    pos
                else
                    // something else, move boxes and move in direction
                    set lw map next '.'
                    set lw map behind 'O'
                    next
                
            | _ -> next
        move lw moves map (n+1) newPos

// get gps value for x,y
let gps (x,y) = x+100*y

// run the whole thing for part 1
let run input =
    let map, moves = parse input
    let lw = Array.IndexOf(map,'\n')+1

    move lw moves map 0 (initialPos lw map)
    map
    |> Array.indexed 
    |> Array.choose(fun (i,c) -> if c = 'O' then Some (toPos lw i) else None)
    |> Array.sumBy gps



run """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"""


run """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""

run ((IO.File.ReadAllText "input/day15.txt").ReplaceLineEndings("\n"))

// part 2

// clear a box (put two '.')
let clearBox lw map p =
    set lw map p '.'
    set lw map (p++(1,0)) '.'

// draw a box at p++dir
let moveBox lw map p dir =
    let next = p++dir
    set lw map next '['
    set lw map (next++(1,0)) ']'

// get the coordinate of the box 
let getBox lw map p =
    if get lw map p = ']' then
        p++(-1,0)
    else 
        p

// test if is a box char
let isBox c = c='[' || c=']'

// returns cells pushed by box at position b in given direction
let pushes b dir =
    match dir with
    | -1,0 -> [b++(-1,0)]
    | 1, 0 -> [b++(2,0)]
    | 0,-1 -> [b++(0,-1); b++(1,-1) ]
    | 0,1 -> [b++(0,1); b++(1,1) ]
    | _ -> failwith "Invalid direction"


// find all boxes pushed if can be pushed, or returns None
// stack contains cells to check
// pushed contains boxes pushed so far
let rec findBedind lw map dir stack pushed =
    match stack with
    | [] ->
        // all boxes could be pushed, return them
        Some pushed
    | h :: rest ->
        // there are some cells to check, get che char from cell
        let c' = get lw map h
        if isBox c' then
            // this is part of a box, get box coordinates
            let b = getBox lw map h
            // see what cells this box pushes
            let next = pushes b dir
            // add them to cells to check
            findBedind lw map dir (next @ rest) (b::pushed)
        elif c' = '#' then
            // this is a wall, nothing can move
            None
        else
            // this is an empty space, continue to check remaining cells
            findBedind lw map dir rest pushed




// move iteration for part 2
let rec move2 lw map (moves: string) n pos =
    if n < moves.Length then
        // get the direction
        let dir = getDir moves n
        // next position if can move 
        let next = pos++dir

        let newPos =
            // check next cell to see which boxes are pushed (if any) and if we can be blocked by a wall
            match findBedind lw map dir [next] [] with
            | Some pushed ->
                // movement is possible in this direction
                // clear all boxes
                for b in pushed do
                    clearBox lw map b
                // draw all new boxes 
                for b in pushed do
                    moveBox lw map b dir
                // move in direction
                next
            | None ->
                // the movement is blocked
                pos

        move2 lw map moves (n+1) newPos


let run2 input =
    let map', moves = parse input
    let map =
        // build map with everything scaled
        [| for c in map' do
            match c with
            | '#' -> yield! [|'#';'#'|]
            | 'O' -> yield! [| '[';']'|]
            | '@' -> yield! [| '@';'.'|]
            | '.' -> yield! [| '.';'.'|]
            | '\n' -> yield! [|'\n'|]
            | _ -> failwith "Unknown char"
        |]

    let lw = Array.IndexOf(map,'\n')+1

    move2 lw  map moves 0 (initialPos lw map)
    // map |> String |> printfn "%s"
    map
    |> Array.indexed 
    |> Array.choose(fun (i,c) -> if c = '[' then Some (toPos lw i) else None)
    |> Array.sumBy gps



// run2 """#######
// #...#.#
// #.....#
// #..OO@#
// #..O..#
// #.....#
// #######

// <vv<<^^<<^^"""

// run2 """##########
// #..O..O.O#
// #......O.#
// #.OO..O.O#
// #..O@..O.#
// #O#..O...#
// #O..O..O.#
// #.OO.O.OO#
// #....O...#
// ##########

// <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
// vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
// ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
// <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
// ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
// ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
// >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
// <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
// ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
// v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""


run2 ((IO.File.ReadAllText "input/day15.txt").ReplaceLineEndings("\n"))