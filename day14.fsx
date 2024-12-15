open System

let rx = Text.RegularExpressions.Regex @"p=(?<px>\d+),(?<py>\d+) v=(?<vx>-?\d+),(?<vy>-?\d+)"
let input = 
    IO.File.ReadAllLines("input/day14.txt")
//     """p=0,4 v=3,-3
// p=6,3 v=-1,-3
// p=10,3 v=-1,2
// p=2,0 v=2,-1
// p=0,0 v=1,3
// p=3,0 v=-2,-2
// p=7,6 v=-1,-3
// p=3,0 v=-1,-2
// p=9,3 v=2,3
// p=7,3 v=-1,2
// p=2,4 v=2,-3
// p=9,5 v=-3,-3""".Split('\n')
    |> Array.map (fun l ->
        let m = rx.Match(l)
        let px = int m.Groups["px"].Value 
        let py = int m.Groups["py"].Value 
        let vx = int m.Groups["vx"].Value 
        let vy = int m.Groups["vy"].Value 
        (px,py), (vx,vy)
    )

let exampleRoom = (11,7)
let room = (101,103)

let (++) (x1,y1) (x2,y2) = x1+x2,y1+y2
let ( ** ) (x,y) n = x*n,y*n

let (%+) n d = 
    let r = n%d
    if r >= 0 then
        r
    else
        r+d
let (%%) (x,y) (w,h) = x%+w,y%+h

let futur room t (p,v)  = (p ++ (v**t)) %% room

let positions= 
    input
    |> Array.map (futur room 100)


let trans (p1,p2) v = p1++v,p2++v
let isIn ((l,t),(r,b)) (x,y) = x >= l && x < r && y >= t && y < b


let saftyQuadrant (rx,ry) positions =
    let hx = rx/2, 0
    let hy = 0, ry/2
    let q1 = ((0,0),hx++hy)
    let q2 = trans q1 (hx++(1,0))
    let q3 = trans q1 (hy++(0,1))
    let q4 = trans q1 (hx++hy++(1,1))
    let rq1 = positions |> Seq.filter (isIn q1) |> Seq.length
    let rq2 = positions |> Seq.filter (isIn q2) |> Seq.length
    let rq3 = positions |> Seq.filter (isIn q3) |> Seq.length
    let rq4 = positions |> Seq.filter (isIn q4) |> Seq.length
    rq1 * rq2 * rq3 *rq4

positions
|> saftyQuadrant room 



let canBeTree room n =
    let robotsOnLines=
        input |> Seq.map (futur room n) 
        |> Seq.distinct
        |> Seq.countBy snd
    let array = Array.zeroCreate (snd room)
    for (y,c) in robotsOnLines do
        array[y] <- c
    let incBy2 = array |> Seq.pairwise |> Seq.map (fun (x,y) -> y-x) |> Seq.filter (fun x -> x=2) |> Seq.length
    incBy2 > 50



let drawRobots (rx,ry) n  =
    let robots = input |> Seq.map (futur room n) |> set
    for y in 0 .. ry-1 do
        for x in 0 .. rx-1 do
            if Set.contains (x,y) robots then
                printf "X"
            else
                printf "."
        printfn ""
    
// the drawing has a higher change to have a column with more than 20 robots
let hasColumns n =  
    input
    |> Seq.map (futur room n)
    |> Seq.countBy fst
    |> Seq.exists (fun (x,c) -> c > 20)

// the drawing has a higher change to have a line with more than 20 robots
let hasLines n =  
    input
    |> Seq.map (futur room n)
    |> Seq.countBy snd
    |> Seq.exists (fun (y,c) -> c > 20)

// if the easter egg appears at t, every 101s (the width of the room), all robots
// will be at the same x than at t... just not at the same y.
// we can find the 1st moment where they are in the right columns
let tcols = Seq.initInfinite id |> Seq.find hasColumns
// same thing for y every 103s. 
let tlines = Seq.initInfinite id |> Seq.find hasLines 

// we now the time of the easter egg has the following constraints:
// t = tlines + x * h
// t = tcols + x * w 
// 
// 0 = tlines-tcols + x * (h-w)
// x = (tcols-tlines)/(h-w)
let w = fst room
let h = snd room
let x = (tcols-tlines)/(h  - w) 
let t = (tcols + x * w) %+ (w*h)  // the intersection is in the past, but it repeats every w*h seconds

drawRobots room t


// this version is more brute force
let t2 = Seq.initInfinite id |> Seq.find (fun n ->  hasColumns n && hasLines n)


drawRobots room t2