open System
let codes =
    IO.File.ReadAllLines("input/day21.txt")


// returns the coordinate for key c on num pad
let numPos c=
    match c with
    | '0' -> 1,0
    | 'A' -> 2,0
    | _ ->
        let d = c - '1' |> int
        (d%3), d/3 + 1 

// the position of the gap on the num pad
let numGap = 0,0


// returns the coordinates for key c on remote pad
let remotePos c =
    match c with
    | '^' -> 1,1
    | 'A' -> 2,1
    | '<' -> 0,0
    | 'v' -> 1, 0
    | '>' -> 2, 0

// the coordinates of the gap on remote pad
let remoteGap = 0,1


// the init position on num pad ('A')
let initNumPos  = numPos 'A'
// the init position on remote pad ('A')
let initRemotePos = remotePos 'A'

// direction vector between two points
let (--) (x,y) (x',y') = x-x',y-y'

// add two vectors
let (++) (x,y) (x',y') = x+x',y+y'

// list possible moves
// pad is a function that returns coordinates for a key
// gap is the coordinates of the gap on the pad
// pos is the starting position
// code is the code to compose
// moves is the result moves (in reverse order), starts with [[]]
let rec numMoves pad gap pos code  moves =
    match code with
    | [] -> moves |> List.map List.rev // reverse codes
    | c :: tail ->
        // the destination on the pad for this key
        let newPos = pad c
        // the direction vector
        let x,y = newPos -- pos 
        // the horizontal move direction
        let moveX = 
            if x > 0 then '>'
            else  '<'
        // the vertical move direction
        let moveY =
            if y > 0 then '^'
            else 'v'
        // number of horizontal moves
        let cx = abs x
        // number of vertical moves
        let cy = abs y
        
        let moves = 
            if cx = 0 then
                if cy = 0 then
                    // no move -> just press A, add it to all combinations
                    [ for m in moves do
                        'A' :: m ]
                else
                    // move vertically only, press A after moves
                    [ for m in moves do
                        [ 'A'
                          for _ in 1 .. cy do moveY
                          yield! m] ]
            elif cy = 0 then
                // move horizontally only
                [ for m in moves do
                    [ 'A'
                      for _ in 1 .. cx do moveX 
                      yield! m ] ]
            else 
                // move in both directions
                [ for m in moves do
                    // start vertically, check it's not going through the gap
                    if pos ++ (0,y) <> gap then
                        [ 'A'
                          for _ in 1 .. cx do moveX
                          for _ in 1 .. cy do moveY 
                          yield! m]
                    // start horizontally, check it's not going through the gap
                    if pos ++ (x,0) <> gap then
                        [ 'A'
                          for _ in 1 .. cy do moveY 
                          for _ in 1 .. cx do moveX
                          yield! m]

                ]
        numMoves pad gap newPos tail moves

// convert path to string
let print keys = keys |> List.toArray |> String         
            
// extract digits from code
let compNum (code: string) = 
    code[0..2]     |> int

// Part 1
codes
|> Array.map (fun code ->
    // get the moves to compose the code on numeric pad
    let s =  numMoves numPos numGap initNumPos  (Seq.toList code) [[]]
    // get the moves on the remote
    let s2 = s |> List.collect (fun keys -> numMoves remotePos remoteGap  initRemotePos keys [[]] )
    // get the moves on the second remote
    let s3 = s2 |> List.collect (fun keys -> numMoves remotePos remoteGap initRemotePos keys [[]] )
    // find the minimum length
    let minLen = s3 |> List.map List.length |> List.min

    // the complexity
    compNum code * minLen
)
|> Array.sum


// Part 2
// Of course, this strategy could not work for part 2 with 25 remotes
// as each move will, on the next remote, produce a sequence starting from A and ending on A,
// each of this sequence can be isolated

// returns the possible sequences of move to go from key c1 to c2 on remote
let pairMove c1 c2 = numMoves remotePos remoteGap (remotePos c1)  [c2] [[]] |> List.map print

open System.Collections.Generic

// findMin returns the minimum sequence for path at given depth,
// it uses a cache to avoid recomputing similat sequences.
// this caches is indexed by (depth,sequence)
let rec findMin depth (path: string) (mins: Dictionary<int*string, int64>) =
    if depth = 0 then
        // at depth 0, the minimum length is the length of the path
        int64 path.Length
    else
        // try to get min for this path at same depth
        match mins.TryGetValue((depth, path)) with
        | true, v -> v // yes, just return it
        | false, _ ->
            // no, compute..
            // for this we decompose the path in smaller paths from a key to the next
            // (add A in front to start from A)

            let minLength =
                ("A" + path)
                |> Seq.pairwise
                |> Seq.sumBy (fun (c1,c2) -> 
                    // for this two consecutive keys, get possible paths
                    let subPaths = pairMove c1 c2
                    // compute the min for each of the path at next depth, and keep the minimum
                    subPaths |> Seq.map (fun p -> findMin (depth-1) p mins) |> Seq.min
                    ) // sum the min length for each key pair to get total min length
            // add min length to the cache
            mins.Add((depth, path), minLength)
            minLength

// get the min length for numeric code
let codeMin depth code =
    // get all moves on numeric pad for the code
    let moves = numMoves numPos numGap initNumPos  (List.ofSeq code) [[]] |> List.map print
    let mins = Dictionary()
    // for each sequence of moves, get the min length, and keep the smallest
    moves |> List.map (fun p -> findMin  depth p mins) |> Seq.min


// compute total complexity
codes
|> Array.sumBy (fun code ->
    codeMin 25 code * int64 (compNum code)
)