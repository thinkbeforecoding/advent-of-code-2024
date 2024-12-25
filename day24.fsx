// --- Day 24: Crossed Wires ---
// https://adventofcode.com/2024/day/24

open System

// Represent a gate
// inputs are included this makes code more homogenous
type Gate =
    | And of string * string
    | Or of string * string
    | Xor of string * string
    | Input of int

let gates =
    let lines = IO.File.ReadAllLines("input/day24.txt")
    // all inputs as a pair (name, Input value)
    let inputs= 
        lines 
        |> Seq.takeWhile (not << String.IsNullOrEmpty)
        |> Seq.map (fun l -> l[0..2], Input (int l[5..5]))

    // all gates as a pair (name, Gate(x,y))
    let gates =
        lines
        |> Seq.skipWhile (not << String.IsNullOrEmpty)
        |> Seq.skip 1
        |> Seq.map (fun l ->
            match l.Split(' ') with
            | [| i1; "AND"; i2; "->";  out|] -> 
                out,And(i1,i2)
            | [| i1; "OR"; i2; "->";  out|] -> 
                out,Or(i1,i2)
            | [| i1; "XOR"; i2; "->";  out|] -> 
                out,Xor(i1,i2)
            | _ -> failwithf "Invalid line '%s'" l
        )

    // make a map of gates and input
    Seq.append inputs gates |> Map.ofSeq


// evaluate a gate (inputs are considered as gates with constant value)
let rec eval o =
    match gates[o] with
    | And(x,y) -> eval x &&& eval y
    | Or(x,y) -> eval x ||| eval y
    | Xor(x,y) -> eval x ^^^ eval y
    | Input x -> x

// Part 1 :: evaluates all z.., each 1 as a bit
gates.Keys |> Seq.filter (fun o -> o[0] = 'z')
|> Seq.map eval
|> Seq.mapi (fun i x -> (int64 x <<< i))  // x is 0 or 1, shift it to put at the correct bit place
|> Seq.sum


// swap two gates output in the gates map
let swap k1 k2 gates =
    let v1 = Map.find k1 gates
    let v2 = Map.find k2 gates
    gates |> Map.add k1 v2 |> Map.add k2 v1

// This are the necessary swaps for this input, I found them
// manually with the help of the following functions
let gates2 = 
    gates
    // |> swap "z10" "gpr" 
//     |> swap "z21" "nks" 
//     |> swap "z33" "ghp"
//     |> swap "krs" "cpm"


// this is a variant of eval
// vx and vy are two 64 bits values
// that are used to evaluate inputs xnn and ynn
// xnn returns bits from vx
// and ynn returns bits from vy
let rec eval2 vx vy o =
    match gates2[o] with
    | And(x,y) -> eval2 vx vy x &&& eval2 vx vy y
    | Or(x,y) -> eval2 vx vy x ||| eval2 vx vy y

    | Xor(x,y) -> eval2 vx vy x ^^^ eval2 vx vy y
    | Input x -> 
        if o[0] = 'x' then
            (vx >>> int o[1..]) &&& 1L
        else
            (vy >>> int o[1..]) &&& 1L

// since the circuit should be an adder, 
// using eval2 with x and y and putting the z bits together
// should perform a 'add' operation
let add x y =
    gates2.Keys |> Seq.filter (fun o -> o[0] = 'z')
    |> Seq.map (eval2 x y)
    |> Seq.mapi (fun i x -> int64 x * (1L <<< i))
    |> Seq.sum

// here, we check where problems happen:
// we add x=0 and y=1 bit 
// the result should be y, however it fails at some positions
// at n=10, n=21, n=33 and n=39
for n in 0 .. 44 do
    (add 0L (1L <<< n)) = (1L<<<n) |> printfn "%d %A" n

// quite similar, both x and y are a bit a position n,
// the sum should be one bit a position n+1,
// but here again it fails
for n in 0 .. 44 do
    (add (1L <<< n) (1L <<< n)) = (1L<<<(n+1)) |> printfn "%d %A" n


// the output z10 is directly And "x10" "y10"
gates["z10"]
// this is not correct, this is the gate to compute the carry to z11
// what should be here is an Xor (carry x09 y09) (Xor x10 y10)
// we can look for this pattern

// this active patterns make it possible to find gates that are input from other gates
let (|Gate|) x =
    gates2[x]

// we can check that all full adders have the correct structure except the ones at the detected positions
for i in 1 .. 44 do
    match gates2[$"z%02d{i}"] with
    | Xor(Gate(Xor(x,y)),_) when (x = $"x%02d{i}" && y = $"y%02d{i}") || (y = $"x%02d{i}" && x = $"y%02d{i}")  -> 
        printfn "%d OK" i
    | Xor(_,Gate( Xor(x,y))) when (x = $"x%02d{i}" && y = $"y%02d{i}") || (y = $"x%02d{i}" && x = $"y%02d{i}") -> 
        printfn "%d OK" i
    | _ -> printfn "%d Nope" i  


// first, try to find the pattern that should be for z10
let findAdder i =
    let xi = $"x%02d{i}"
    let yi = $"y%02d{i}"
    gates2 |> Map.toSeq |> Seq.filter (fun (o,g) ->
        match g with
        | Xor(_,Gate( Xor(x,y))) 
        | Xor(Gate(Xor(x,y)),_)
            when (x = xi && y = yi) || (y = xi && x = yi)  -> true
        | _ -> false
    )
// its "gpr", we can swap it in gates2, and check that it's not bogus at bit 10 anymore
findAdder 10

// the 2nd problem is at 21
gates2["z21"]
// here it's And ptd scj
// the adder is actually nks, z21 and nks should be swapped
findAdder 21

// z33 is Or jtg, trf
gates2["z33"]
// the adder is ghp, z33 and ghp should be swapped
findAdder 33

// z39 is an XOr krs mhr
gates2["z39"]
// krs is And y39 x39, which should be the carry for z40.. this is not correct, here should be XOr x39 y39
gates2["krs"]

// XOr x39 y39 is "cpm"
gates2 |> Map.toSeq |> Seq.filter (fun (o,g) ->
    match g with
    | Xor(x,y) when (x = $"x39" && y = $"y39") || (y = $"x39" && x = $"y39")  -> 
        true
    | Xor(x,y) when (x = $"x39" && y = $"y39") || (y = $"x39" && x = $"y39") -> 
        true
    | _ -> false
)

// so krs and cpm should be swapped
// now we can apply all the swaps and check that add ition is behaving
// as intended


// the result is the concatenation:
set ["z10"; "gpr"; "z21" ; "nks" ; "z33"; "ghp" ;"krs"; "cpm"]
|> String.concat ","