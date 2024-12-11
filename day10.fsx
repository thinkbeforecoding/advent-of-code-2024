open System

let input = IO.File.ReadAllText("input/day10.txt").ReplaceLineEndings("\n")
// let input = 
//     """..90..9
// ...1.98
// ...2..7
// 6543456
// 765.987
// 876....
// 987...."""

let w = input.IndexOf('\n')
let lw = w+1
let h = input.Split('\n', StringSplitOptions.RemoveEmptyEntries).Length

let inbounds (x,y) = x >= 0 && x < w && y >= 0 && y < h
let toIndex (x,y) = x+y*lw
let toPos index = index%lw, index / lw

let get p = 
    if inbounds p then
        input[toIndex p]
    else
        '.'


let rec findHeads index  all =
    if index < input.Length then
        let c = input[index]
        if c = '0' then
            findHeads (index+1) (toPos index :: all)
        else
            findHeads (index+1) all
    else
        all
        

findHeads 0 []

let (++) (x1,y1) (x2,y2) = x1+x2, y1+y2

let N = 0,-1
let E = 1,0
let S = 0,1
let W = -1,0
let nextDir c pos dir stack =
    let n = pos ++ dir
    let nc = get n
    if int nc = int c+1 then
        n :: stack
    else
        stack
let rec trail' stack ends paths =
    match stack with
    | [] -> ends,paths
    | pos :: tail ->
        let c = get pos
        
        if c = '9' then
            trail' tail (Set.add pos ends) (paths+1)
        else
            let stack =
                tail
                |> nextDir c pos N 
                |> nextDir c pos E
                |> nextDir c pos S
                |> nextDir c pos W
            trail' stack ends paths

let trails pos = trail' [pos] Set.empty 0
            

findHeads 0 [] 
|> List.sumBy (trails >> fst >> Set.count) 

findHeads 0 [] 
|> List.sumBy (trails >> snd ) 


    
