// --- Day 10: Hoof It ---
// https://adventofcode.com/2024/day/10

open System

// read the map as a string
let input = IO.File.ReadAllText("input/day10.txt").ReplaceLineEndings("\n")

// the width of the map
let w = input.IndexOf('\n')
// the distance to next line
let lw = w+1
// the height of the map
let h = input.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

// check if x,y is in bounds
let inbounds (x,y) = x >= 0 && x < w && y >= 0 && y < h
// convert x,y to string index
let toIndex (x,y) = x+y*lw
// convert string index to x,y
let toPos index = index%lw, index / lw

// get character at position x,y, return '.' if out of map
let get p = 
    if inbounds p then
        input[toIndex p]
    else
        '.'


// get all heads
// find the '0's and convert index to x,y 
let heads =
    input
    |> Seq.mapi (fun i c -> if c = '0' then Some(toPos i) else None)
    |> Seq.choose id
    |> Seq.toList

// add vectors
let (++) (x1,y1) (x2,y2) = x1+x2, y1+y2

// the 4 main directions
let N = 0,-1
let E = 1,0
let S = 0,1
let W = -1,0

// find the next direction
// c is the current trail char (starts at '0')
// pos is the x,y position on the map,
// dir is the direction to test
// adds the next position in given direction to the stack if
// it follows current char
let nextDir c pos dir stack =
    let n = pos ++ dir
    let nc = get n
    if int nc = int c+1 then
        n :: stack
    else
        stack
// explor trails for part 1 and 2 
// ends contains the all the ends, paths contains the path count
let rec trail' stack ends paths =
    match stack with
    | [] ->
        // we finished the exploration
        // returns found ends ands paths
        ends,paths
    | pos :: tail ->
        // test the trail at position
        // get the character there
        let c = get pos
        
        if c = '9' then
            // its a 9, we found an end,
            // test others trails/path, but add this to the ends, and add one path to total
            trail' tail (Set.add pos ends) (paths+1)
        else
            // this is not a 9,
            // explore the 4 directions, and add them to stack for further exploration if they
            // contain current char + 1
            let stack =
                tail
                |> nextDir c pos N 
                |> nextDir c pos E
                |> nextDir c pos S
                |> nextDir c pos W

            // continue exploration
            trail' stack ends paths

// call the recursive function with start parameters
let trails pos = trail' [pos] Set.empty 0
            

// Part 1
// sum count of ends for all heads
heads 
|> List.sumBy (trails >> fst >> Set.count) 

// Part 2
// sum path count for all heads
heads
|> List.sumBy (trails >> snd ) 


    
