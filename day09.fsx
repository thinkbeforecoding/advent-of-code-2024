// --- Day 9: Disk Fragmenter ---
// https://adventofcode.com/2024/day/9

// convert the input as a list of int (between 0 and 9)
let input =
    System.IO.File.ReadAllText("input/day09.txt")
    |> Seq.map (fun c -> int (c - '0'))
    |> Seq.toList

// the total length of the disk (each number represen a number of sectors)
let totalLen = input |> Seq.sum
// initialize an array of the size of the disk
let disk = Array.zeroCreate<int> totalLen
// fill the disk with file id, and free space with -1
let rec fill  pos id sectors=
    match sectors with
    | [] -> ()
    | file :: free :: rest ->
        for i in 0 .. file-1 do
            disk[pos+i] <- id
        for i in 0 .. free-1 do
            disk[pos+file+i] <- -1 
        fill (pos+file+free) (id+1) rest
    | [file] ->
        for i in 0 .. file-1 do
            disk[pos+i] <- id        

fill 0 0 input 
disk

// compact the disk recursively
//


let rec compact  disk =
    // find index of first free sector
    let firstFree = disk |> Array.findIndex(fun x -> x= -1)
    // find index of last occupied sector
    let last = disk |> Array.findIndexBack(fun x -> x <> -1)
    if firstFree > last then
        // all sectors are now contiguous at the begining of the disk
        disk
    else
        // swap sectors
        disk[firstFree]<-disk[last]
        disk[last] <- -1

        // try next sector
        compact disk

// comput the disk checksum
let checksum disk =
    disk
    |> Seq.indexed
    |> Seq.sumBy (fun (pos,id) ->
        if id >= 0 then
            int64 pos * int64 id
        else
            0L
    )
compact disk
|> checksum


// Part 2
// the previous representation doesn't work well with this part
// model a disk space, with its size, and number if a file
type Space =
    | File of int * int
    | Free of int
    member this.Size =
        match this with 
        | File(_,s) -> s 
        | Free(s) -> s
        

// fills the disk
let rec fill2' pos id sectors (disk: Space[]) =
    match sectors with
    | [] ->
        // all sectors have been added
        disk
    | file :: free :: rest ->
        // the two first numbers represent a file and a free space
        // add them to disk at given pos
        disk[pos] <- File(id, file)
        disk[pos+1] <- Free free
        fill2' (pos+2) (id+1) rest disk
    | [file] ->
        // at the end, there is a single file space left
        disk[pos] <- File(id, file)
        disk

// allocate array for disk, and fill it
let fill2 (sectors: int list) = 
    let disk = Array.zeroCreate (sectors.Length)
    fill2' 0 0 sectors disk
    


// merge free space in disk
let merge (disk: Space[]) =
    seq {
        let mutable last = Free 0
        for i in 0 .. disk.Length-1 do
            match last, disk[i] with
            | Free(llen), Free(len) ->
                // the last seen space was free, merge with curren free space
                last <- Free(llen + len)
            | Free 0, s ->
                // there was no free space before, do nothing
                last <- s
                
            | _, s ->
                // there was free space, before, but curren is file,
                // return aggregated free space
                last
                last <- s
        // return the last space
        last
    } |> Seq.toArray


// compact the disk
let rec compact2 id (disk2: Space[]) =
    if id >= 0 then
        // compact file
        // get the position of the file on disk
        let fileIndex = disk2 |> Array.findIndex (function File(i,_) -> i = id | _ -> false)
        // get the file
        let file = disk2[fileIndex]

        // find the first free space large enough for file size
        let firstFree =
            disk2 |> Array.tryFindIndex(function
                | File _ -> false
                | Free freeLen -> freeLen >= file.Size 
            )

        let disk2 =
            match firstFree with
            | Some n when n < fileIndex ->
                // free space is before file, we can move it
                // get actual free space
                let free = disk2[n]
                // put file there, and free previous file space
                disk2[n] <- file
                disk2[fileIndex] <- Free(file.Size)
                let disk2 =
                    if free.Size > file.Size then
                        // there is still free space after file at new position
                        // insert it
                        Array.insertAt(n+1)  (Free(free.Size - file.Size)) disk2
                    else
                        disk2

                // merge free space before next operation
                merge disk2
            | _ -> 
                // no big enough free space, just do nothing 
                disk2
        // move to next file id
        let id = id-1
        compact2 id disk2

    else
        // all files have been compacted
        disk2

// expand the Spaces as individual sectors
let expand disk =
    [ for i in 0 .. Array.length disk - 1 do
        match disk[i] with
        | File(id, len) ->
            for j in 0 .. len-1 do
                id
        | Free(len) ->
            for j in 0 .. len-1 do
                -1
    ]


// create disk and merge it (to remove Free 0 etc)
let disk2 = fill2  input |> merge

// find id of last file
let lastFile = disk2 |> Seq.choose(function File(id,_) -> Some id | _ -> None) |> Seq.max

// compact the disk starting from last file, 
// then expand it to compute the checksum
compact2 lastFile disk2
|> expand
|> checksum
