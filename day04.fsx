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

let input = IO.File.ReadAllLines("input/day04.txt")
let height = input.Length
let width = input.Length
let getChar (x,y) =
    if x >= 0 && x < width && y >= 0 && y < height then
        (input[y][x])
    else
        '!'

let check (dx,dy) (x,y) = 
    if getChar(x,y) = 'X' && getChar(x+dx,y+dy) = 'M' && getChar(x+dx*2,y+dy*2) = 'A' && getChar(x+dx*3,y+dy*3) = 'S' then
        1
    else
        0
let checkAll p =
    check (1, 0) p + check (0,1) p + check (-1,0) p + check(0,-1) p 
    + check (1, 1) p + check(1,-1) p + check (-1,-1) p + check (-1,1) p

[
for y in 0 .. height-1 do
    for x in 0..width-1 do
        (x,y)
] |> List.sumBy checkAll

// part 2

let xcheck (dx1,dy1) (dx2,dy2) (x,y)=
    if 
        getChar (x,y) = 'A'
        && getChar(x-dx1,y-dy1) = 'M' && getChar(x+dx1,y+dy1) = 'S'
        && getChar(x-dx2,y-dy2) = 'M' && getChar(x+dx2,y+dy2) = 'S'
    then 1
    else 0

let xcheckAll p =
    xcheck (1,1) (1,-1) p 
    + xcheck(1,1) (-1,1) p 
    + xcheck (-1,-1) (1,-1) p
    + xcheck (-1,-1) (-1,1) p

for y in 0 .. height-1 do
    for x in 0..width-1 do 
        printf "%d" (xcheckAll(x,y))
    printfn ""
[
for y in 0 .. height-1 do
    for x in 0..width-1 do
        (x,y)
] |> List.sumBy xcheckAll