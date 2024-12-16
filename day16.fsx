open System
open System.Collections.Generic

let input =
    IO.File.ReadAllText("Input/day16.txt").ReplaceLineEndings("\n")

let w= input.IndexOf('\n')
let lw = w+1

let h = input.Split("\n", StringSplitOptions.RemoveEmptyEntries).Length

// convert index in string to x,y coordinates
let toIndex (x,y) = x+lw*y
// convert x,y to index
let toPos index = index%lw,index/lw

// add vectors
let (++) (x,y) (x',y') = x+x',y+y'

// get char at x,y
let get p = input[toIndex p]

// start position
let start = toPos (input.IndexOf 'S')
// end position
let end' = toPos (input.IndexOf 'E')

// rotate vector right
let rotR (x,y) = -y,x
// rotate vector left
let rotL (x,y) = y,-x

// East vector (starting direction)
let E = (1,0)
/// Compute the manhattan distance
let manhattan (x,y) (x',y') = abs (x'-x) + abs (y' - y)
// true if cells are neither in same column nor same line
let diag (x,y) (x',y') = x<>x' || y<>y'

// check if a move can be done in given direction, and add it to stack
let tryDir pos dir score visited (stack: PriorityQueue<_,int>) =
    let next = pos ++ dir
    match get next with
    | '#' -> () // this is a wall, do nothing
    | _ -> 
        // hestimate cost to reach end = manhattan distance + 1000 if a turn is required
        let p = manhattan next end' + if diag pos end' then 1000 else 0
        stack.Enqueue((next, dir, score, visited),  (score + p) )
            



// run the search.
// the priority queue contains:
// position, direction, score and visited points
// best contains for each point the best score already seen for that point
let rec run (stack: PriorityQueue<_,int>) best =
    if stack.Count = 0 then
        failwithf "Not found"
        
    else
        let pos, (dir: int*int), (score: int), (visited: Set<int*int>) = stack.Dequeue()
        // add position to visited points
        let visited = Set.add pos visited
        if get pos = 'E' then
            // we reached the end, this is necessary the shortest path
            score
        else
            // get best score for current position
            let bestHere = Map.tryFind pos best |> Option.defaultValue Int32.MaxValue
            if bestHere <= score then
                // current test is not better, no need to explore further
                printfn "%d %d" bestHere score
                run stack best
            else
                // add current score as best score
                let best = Map.add pos score best
                
                // add each direction with specific scores
                // for front, left and right. They will be explored in order due to priority queue
                tryDir pos dir (score+1)  visited stack
                tryDir pos (rotL dir) (score+1001) visited stack
                tryDir pos (rotR dir) (score+1001) visited stack 
                run stack best

let go() =
    let stack = PriorityQueue<(int*int)*(int*int)*int*Set<int*int>,int>()
    stack.Enqueue((start, E, 0, Set.empty), 0)
    run stack Map.empty

let bestScore = go()

let print visited =
    for y in 0 .. h-1 do
        for x in 0 .. w-1 do
            if Set.contains(x,y) visited then
                printf "O"
            else
                printf "%c" (get (x,y))
        printfn ""

// this version is quite similar to previous one, but we must not cut too early
// we continue exploring for score equal to best score, we also add direction
// because depending on arrival direction the score can be +1000 (arrive after a turn)
// total is a set that contains all points from paths
let rec run2 (stack: PriorityQueue<_,int>) best  total=
    if stack.Count = 0 then
        total
    else
        let pos, (dir: int*int), (score: int), (visited: Set<int*int>) = stack.Dequeue()
        let visited = Set.add pos visited
        if get pos = 'E' then
            // we reached the end
            if score = bestScore then
                // this is the best score
                printfn "Found path %d (%d)" score visited.Count

                // add visited points to total
                run2 stack best (visited + total)
            else
                // we reach the end but with a longer path,
                // just continue
                run2 stack best total
        else
            // Get best score for this point/direction
            let bestHere, bestVisited = Map.tryFind (pos,dir) best |> Option.defaultValue (Int32.MaxValue, Set.empty)
            if bestHere < score then
                // there is a better score
                // abandon exploration
                run2 stack best total
            else
                // add current score as best score
                let best = Map.add (pos,dir) (score,visited) best
                
                // add directions to explore
                tryDir pos dir (score+1)  visited stack
                tryDir pos (rotL dir) (score+1001) visited stack
                tryDir pos (rotR dir) (score+1001) visited stack 
                
                run2 stack best total

let go2() =
    let stack = PriorityQueue<(int*int)*(int*int)*int*Set<int*int>,int>()
    stack.Enqueue((start, E, 0, Set.empty), 0)
    run2 stack Map.empty Set.empty

go2() |> Set.count


