// --- Day 23: LAN Party ---
// https://adventofcode.com/2024/day/23

open System

// read input as pairs
let input =
    IO.File.ReadAllLines("input/day23.txt")
    |> Array.map (fun l -> 
        match l.Split('-') with
        | [| a;b|] -> a,b 
        | _ -> failwith "Invalid line")


// list of all links in both directions
let links = 
    set input
    + (input |> Seq.map (fun (x,y) -> y,x) |> set )

// the set of neighbors for each nodes
let neighbors = 
    links |> Seq.groupBy fst |> Seq.map (fun (k,v) -> k,set (Seq.map snd v)) |> Map.ofSeq

// every group will appear several times but we deduplicate them with sets
let threeNodesGroups = 
    seq {
        // for each link
        for s,e in links do 
            // we already have 2 computers connected,
            // find the intersections of their neigbors, they form group of 3
            let ds = neighbors[s]
            let es = neighbors[e]
            let thrirds = Set.intersect ds es
            for third in thrirds do
                // check at least one start with t
                if s[0] = 't' || e[0] = 't' || third[0] = 't' then
                    set [ s; e; third]
    } |> set

threeNodesGroups |> Seq.length


// Part 2

// this is a problem to find a maximal clique
// we use the bronKerbosh algorithm for that:
// https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
// it returns all the maximal cliques, and we take the largest one in the end
let nodes = neighbors.Keys |> Set
let rec bronKerbosh r p x (result: ResizeArray<_>) = 
    if Set.isEmpty p && Set.isEmpty x then
        if Set.count r > 2 then
            result.Add r
    else
        let mutable lp = p
        let mutable lx = x
        for v in p do
            bronKerbosh (Set.add v r) (Set.intersect neighbors[v] lp) (Set.intersect neighbors[v] lx) result
        
            lp <- Set.remove v lp
            lx <- Set.add v  lx


// collect all clicks (we keep only those > 2) and find the largest one
let result = ResizeArray()
bronKerbosh Set.empty nodes Set.empty result
result |> Seq.maxBy Set.count |> String.concat ","
