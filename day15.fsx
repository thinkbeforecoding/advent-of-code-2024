open System

let parse (input: string) = 
    // let all = IO.File.ReadAllText("input/day15.txt").ReplaceLineEndings("\n")

    let split = input.IndexOf("\n\n")
    let map = input[0..split] |> Seq.toArray
    let moves = input[split..].Replace("\n","")
    map, moves


let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2

let toPos lw index= index%lw, index/lw
let toIndex lw (x,y) = x+y*lw

let get lw  (map: char[]) p = map[toIndex lw p]
let set lw (map: char[]) p v = map[toIndex lw p] <- v

let getDir (moves: string) i =
    match moves[i] with
    | '<' -> -1,0
    | '^' -> 0, -1
    | '>' -> 1,0
    | 'v' -> 0,1
    | _ -> failwith "Unknown move"

let initialPos lw map = Array.IndexOf(map, '@') |> toPos lw

let rec behindBoxes lw map p dir =
    let next = p++dir
    if get lw map next = 'O' then
        behindBoxes lw map next dir
    else
        next

let rec move lw (moves: string) map n pos =
    if n < moves.Length then

        let dir = getDir moves n
        let next = pos++dir
        let newPos = 
            match get lw map next with
            | '#' -> pos
            | 'O' ->
                let behind = behindBoxes lw map next dir
                if get lw map behind = '#' then
                    pos
                else
                    set lw map next '.'
                    set lw map behind 'O'
                    next
                
            | _ -> next
        move lw moves map (n+1) newPos

let gps (x,y) = x+100*y

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



let moveBox lw map p dir =
    set lw map p '.'
    set lw map (p++(1,0)) '.'
    let next = p++dir
    set lw map next '['
    set lw map (next++(1,0)) ']'

let getBox lw map p =
    if get lw map p = ']' then
        p++(-1,0)
    else 
        p

let isBox c = c='[' || c=']'
let pushes b dir =
    match dir with
    | -1,0 -> [b++(-1,0)]
    | 1, 0 -> [b++(2,0)]
    | 0,-1 -> [b++(0,-1); b++(1,-1) ]
    | 0,1 -> [b++(0,1); b++(1,1) ]
    | _ -> failwith "Invalid direction"


// map |> String |> printfn "%s"
// let lw = Array.IndexOf(map,'\n')+1
// let pos = initialPos lw map
// let n = 0

let rec findBedind lw map dir stack pushed =
    match stack with
    | [] -> Some pushed
    | h :: rest ->
        let c' = get lw map h
        if isBox c' then
            let b = getBox lw map h
            let next = pushes b dir
            findBedind lw map dir (next @ rest) (b::pushed)
        else
            if c' = '#' then
                None
            else
                findBedind lw map dir rest pushed




let rec move2 lw map (moves: string) n pos =
    if n < moves.Length then
        let dir = getDir moves n
        let next = pos++dir

        let c= get lw map next
        let newPos =
            match findBedind lw map dir [next] [] with
            | Some pushed ->
                for b in pushed do
                    moveBox lw map b dir
                next
            | None -> pos
        move2 lw map moves (n+1) newPos


let run2 input =
    let map', moves = parse input
    let map =
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



run2 """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"""

run2 """##########
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


run2 ((IO.File.ReadAllText "input/day15.txt").ReplaceLineEndings("\n"))