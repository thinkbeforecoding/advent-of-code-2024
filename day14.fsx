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
        |> Seq.countBy snd
    robotsOnLines[0] = 1 && robotsOnLines[1] = 3 && robotsOnLines[2] = 5


let drawRobots (rx,ry) n  =
    let robots = input |> Seq.map (futur room n) |> set
    for y in 0 .. ry-1 do
        for x in 0 .. rx-1 do
            if Set.contains (x,y) robots then
                printf "X"
            else
                printf "."
        printfn ""

drawRobots room 21