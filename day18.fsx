open System
open System.Collections.Generic

let input =
    IO.File.ReadAllLines("input/day18.txt")
    |> Array.map (fun l -> 
        match l.Split(",") with
        | [|x;y|] -> int x, int y
        | _ -> failwith "Invalid input"
    )

// take the 1st Kb
let firstKb =
    input |> Array.take 1024 |> set


// check whether given coordinates is free to go 
// warning: width and height are inclusive
let isFree (map,(w,h)) (x,y) =
    x >= 0 && x <= w && y >= 0 && y <= h && not (Set.contains (x,y) map)

let map = (firstKb,(70,70))


// vector addition
let (++) (x,y) (x',y') = x+x',y+y'
// main directions vectors
let N = 0,-1
let E = 1,0
let S = 0,1
let W = -1,0

// check if arrived at destination (1st arg is map)
let arrived (_,e) p = e = p

// compute the manhattan distance between 2 points
let manhattan (x,y) (x',y') = abs(x'-x) + abs(y'-y)

// check if we can move in direction to add to the stack
let tryDir map pos dir steps visited (stack: PriorityQueue<_,_>) =
    let next = pos ++ dir
    if not (Set.contains next visited ) &&  isFree map next then
        // we can move (free and not already visited)
        stack.Enqueue((next,steps+1, visited), steps + 1 + manhattan next (snd map))

// draws the map for diagnostics
let draw ((map,(w,h)) as m) visited =
    for y in 0 .. h do
        for x in 0..w do
            if isFree m (x,y) then
                if Set.contains (x,y) visited then
                    printf "O"
                else
                    printf "."
            else
                printf "#"
        printfn ""



let rec run map best (stack: PriorityQueue<_,_>) =
    if stack.Count = 0 then
        // all has been tried to reach destination, there is no path
        None
    else
        // get next position to try
        let pos,steps, visited = stack.Dequeue()

        // add current position to visited position (to avoid adding points where we already went)
        let visited = Set.add pos visited
        if arrived map pos then
            // we arrived !
            // returns number of steps to go there
            Some steps
        else
            // look if we already came here from another path with a lower number of steps
            let bestHere = Map.tryFind pos best |> Option.defaultValue Int32.MaxValue 
            
            if steps < bestHere then
                // this is the best number of steps, continue exploration
                let best = Map.add pos steps best
                // add all possible direction to stack
                tryDir map pos E steps visited  stack
                tryDir map pos S steps visited  stack
                tryDir map pos N steps visited  stack
                tryDir map pos W steps visited  stack

                run map best stack
            else
                // we already found a better path going through here, abandon exploration
                run map best stack

                    
            
// run on the 1st KB map
let stack = PriorityQueue<(int*int)* int * Set<int*int>, int>()
stack.Enqueue(((0,0),0,Set.empty), 0)
run map Map.empty stack
        
// check if the map is blocked for n bytes
let isBlocked n = 
    // take the n first bytes an create the map
    let bytes = Array.take  n input |> set
    let map = (bytes,(70,70))

    // find a path
    let stack = PriorityQueue<(int*int)* int * Set<int*int>, int>()
    stack.Enqueue(((0,0),0,Set.empty), 0)
    let result = run map Map.empty stack

    // it stopped without finding a path: it's blocked
    result.IsNone 

// finds the number of bytes when it's blocked (by dicotomy)
// lo is the lower bound, is not blocked
// hi is the higher bound, is blocked
let rec findFirstBlocked lo hi =
    // get the middle
    let l  = (lo+hi)/2
    printfn "%d / %d : %d" lo hi l
    // check if it's blocked
    if isBlocked l then
        // yes, the 1st blocked is before
        findFirstBlocked lo l
    else
        if lo+1 = hi then
            // lo is not blocked and hi is blocked
            // so hi is the first
            hi
        else
            // not blocked, the 1st blocked is after
            findFirstBlocked l hi

// get number of bytes when 1st blocked
let first = findFirstBlocked 1025 (input.Length - 1)

// return the last one's coordinates
input[first-1]