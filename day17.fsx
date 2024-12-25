// --- Day 17: Chronospatial Computer ---
// https://adventofcode.com/2024/day/17

open System
#nowarn "57"

// This is the CPU current state
type CPU =
    { A: int64
      B: int64
      C: int64
      
      IP: int
      Program: int[]
      Out: string
      }

// Parse CPU from file
let cpu =
    let lines = 
        IO.File.ReadAllLines("input/day17.txt")
    let reg i = lines[i].Split(":")[^0] |> int
    let a = reg 0
    let b = reg 1
    let c = reg 2
    let prog = (lines[4].Split(":")[^0]).Split(",") |> Array.map int
    { A = a
      B = b
      C = c
      IP = 0
      Program = prog
      Out = ""
    }

// check if cpu halts
let halt cpu = cpu.IP = cpu.Program.Length

// represent Combo value
type Combo =
    | Val of int
    | A
    | B
    | C

// convert number to combo
let (|Combo|) x =
    match x with
    | x when x >= 0 && x <= 3 -> Val x
    | 4 -> A
    | 5 -> B
    | 6 -> C
    | _ -> failwith "Invalid combo"

// get value for combo (from immediate or register)
let value cpu = function
    | Val x -> int64 x
    | A -> cpu.A
    | B -> cpu.B
    | C -> cpu.C

/// print CPU state
let print cpu =
    printfn "--------------------"
    printfn "A: %32B B: %32B C: %32B" cpu.A cpu.B cpu.C
    printfn "IP: %d %A" cpu.IP cpu.Program[cpu.IP.. min(cpu.IP+4) (cpu.Program.Length-1) ]
    printfn "Out: %s" cpu.Out


// Execute next instruction an instruction
let exec ({ A = a; B = b; C = c} as cpu) =
    match cpu.Program[cpu.IP], cpu.Program[cpu.IP+1] with
    | 0, Combo cb -> 
        { cpu with A = a >>> min 31 (int (value cpu cb)) ; IP = cpu.IP+2 }
    | 1, x ->
        { cpu with B = b ^^^ x; IP = cpu.IP+2 }
    | 2, Combo cb ->
        { cpu with B = value cpu cb &&& 0x7 ; IP = cpu.IP+2}
    | 3, x ->
        if a =0 then
            { cpu with IP = cpu.IP+2}
        else
            { cpu with IP = x}
    | 4, _ ->
        { cpu with B = c ^^^ b; IP = cpu.IP+2}
    | 5, Combo cb ->
        { cpu with 
            Out =
                if cpu.Out.Length = 0 then
                    cpu.Out
                else
                    cpu.Out+","
                + string (value cpu cb &&& 7)

            IP = cpu.IP+2 }
    | 6, Combo cb -> 
        { cpu with B = a >>> min 31 (int(value cpu cb)) ; IP = cpu.IP+2 }
    | 7, Combo cb -> 
        { cpu with C = a >>> min 31 (int(value cpu cb)) ; IP = cpu.IP+2 }
    | _ -> failwith "Unknown instruction"

// Run cpu until halt
let rec run cpu =
    if halt cpu |> not then
        run (exec cpu)
    else
        cpu.Out

// Part 1
run cpu |> printfn "%s"




// print the assembly of the program 

let combo =
    function 
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | Val x -> string x

let op cpu ip =
    printf "%02d " ip
    match cpu.Program[ip], cpu.Program[ip+1] with
    | 0, Combo cb -> 
        printfn "adv %s // A = A >>> %s" (combo cb) (combo cb)
    | 1, x ->
        printfn "bxl %d // B = B xor %d" x x
    | 2, Combo cb ->
        printfn "bst %s // B = %s & 7" (combo cb) (combo cb)
    | 3, x ->
        printfn "jnz %d" x
    | 4, _ ->
        printfn "bxc   // B = C xor B"
    | 5, Combo cb ->
        printfn "out %s" (combo cb)
    | 6, Combo cb -> 
        printfn "bdv %s // B = A >>> %s" (combo cb) (combo cb)
    | 7, Combo cb -> 
        printfn "bdv %s // C = A >>> %s" (combo cb) (combo cb)
    | _ -> failwith "Unknown instruction"


for i in 0 .. 2 .. cpu.Program.Length-2 do
    op cpu i

// A contains the ADN
// each step takes 3 digit from a (bits)
// an index is computed as bits xor 3 and 3 bits at this index are taken
// this bits are xor with 6 and output
// A is shifted 3 bits rights 

// Find the value of A to output the program
// a is the value of register A
// b is a mask of bits whose value is already fixed
// p in the position in the program
// n is the number we try (from 0 to 7) at current location
let rec find (a: int64) (b: int64) p n =
    if p = cpu.Program.Length then
        // we reached the end of the program ! Hourra !
        Some a
    elif n >= 8 then
        // we tried all possibilities as current location...
        // trackback
        None
    else
        // the current shift is p*3 since A is shifted 3 right per turn
        let shift = p*3

        // get current bits from a at this place
        let bits = (a >>> shift) &&&7L         // values
        // get mask of bits already determined at this place
        let fixedBits = (b >>> shift) &&& 7L    // mask of bits already sets

        // check if n (the value we want to put in these 3 bits) is compatible
        // with existing bits
        if (int64 n &&& fixedBits) <> (bits &&& fixedBits) then
            // nope... 
            // some bits have been already set and are incompatible with n
            // try with n+1
            find a b p (n+1)
        else
            // yes, n is compatible !
            // save a and b in case of failure
            let orga = a
            let orgb = b
            // the bits we want are n
            let bits = int64 n
            // we integrate them in a  and indicate they are fixed now in b
            let a = a ||| (bits <<< shift)
            let b = b ||| (7L <<< shift)

            // we compute the index where data is taken to output next value
            let index = int (bits^^^3)
            // At this place we need to find this so that it output the program opcode
            let newBits = bits ^^^ 6 ^^^ cpu.Program[p]
            // we put them in place (like they would be in A)
            let shiftedNewBits = (int64 newBits <<< (index+shift))
            // the mask of the bits to set
            let newBitsMask = 7L <<< (index + shift)

            // we check that these bits, at the place where some bits are fixed in A
            // ar actually equal to the bits in A.
            if (shiftedNewBits &&& b) <> (a &&& newBitsMask &&& b) then
                // nope...
                // bits that are already set at index will not output the correct opcode
                find orga orgb p (n+1)
            else
                // great !
                // those bits can fit here
                // set them in A and the mask
                let a = a ||| shiftedNewBits
                let b = b ||| newBitsMask

                // now we can try to move to next position and opcode
                match find a b (p+1) 0 with
                | Some v -> 
                    // We reached the end, and found a solution
                    Some v
                | None ->  
                    // no solution has been found further, this is a dead end
                    // try next n for same position
                    find orga orgb p (n+1)

// Part 2
let a =
    find 0L 0L 0 0 
    |> function
        | Some a -> a
        | None -> failwithf "No solution"


// check the result is the program
run { cpu with A = a }  =  (cpu.Program |> Seq.map string |> String.concat ",")

