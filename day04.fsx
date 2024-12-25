open System
// let input = 
//     """MMMSXXMASM
// MSAMXMSMSA
// AMXSXMAAMM
// MSAMASMSMX
// XMASAMXAMM
// XXAMMXXAMA
// SMSMSASXSS
// SAXAMASAAA
// MAMMMXMMMM
// MXMXAXMASX""".Split("\n")

// Read the full file as a string
let input = IO.File.ReadAllText("input/day04.txt").ReplaceLineEndings("\n")
// width of the map
let width = input.IndexOf('\n')
// distance to next line
let lineWidth = width+1
// height of the map
let height = input.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

// get char at (x,y), returns ❌ if outside of the map
let getChar (x,y) =
    if x >= 0 && x < width && y >= 0 && y < height then
        input[x+y*lineWidth]
    else
        '❌'

// check that, from (x,y), letters are XMAS when moving along (dx,dy),
// returns 1 if true
let check (dx,dy) (x,y) = 
    if getChar(x,y) = 'X' && getChar(x+dx,y+dy) = 'M' && getChar(x+dx*2,y+dy*2) = 'A' && getChar(x+dx*3,y+dy*3) = 'S' then
        1
    else
        0

// check all directions and sum results
let checkAll p =
    check (1, 0) p + check (0,1) p + check (-1,0) p + check(0,-1) p 
    + check (1, 1) p + check(1,-1) p + check (-1,-1) p + check (-1,1) p

// generates all coordinates and sum all results
[ for y in 0 .. height-1 do
    for x in 0 .. width-1 do
        (x,y)
] |> List.sumBy checkAll

// part 2

// check the cross pattern, where the A should be at (x,y)
// vectors (dx1,dy1) and (dx2,dy2) indicates the direction of the M -> S
let xcheck (dx1,dy1) (dx2,dy2) (x,y)=
    if 
        getChar (x,y) = 'A'
        && getChar(x-dx1,y-dy1) = 'M' && getChar(x+dx1,y+dy1) = 'S'
        && getChar(x-dx2,y-dy2) = 'M' && getChar(x+dx2,y+dy2) = 'S'
    then 1
    else 0

// check all four patterns, and count them
let xcheckAll p =
    xcheck (1,1) (1,-1) p 
    + xcheck(1,1) (-1,1) p 
    + xcheck (-1,-1) (1,-1) p
    + xcheck (-1,-1) (-1,1) p


// Sum the patterns at all positions
[ for y in 0 .. height-1 do
    for x in 0 .. width-1 do
        (x,y)
] |> List.sumBy xcheckAll