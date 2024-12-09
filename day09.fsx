let input =
    // "2333133121414131402"
    System.IO.File.ReadAllText("input/day09.txt")
    |> Seq.map (fun c -> int (c - '0'))
    |> Seq.toList

let totalLen = input |> Seq.sum
let disk = Array.zeroCreate<int> totalLen
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

let rec compact  disk =
    let firstFree = disk |> Array.findIndex(fun x -> x= -1)
    let last = disk |> Array.findIndexBack(fun x -> x <> -1)
    if firstFree > last then
        disk
    else
        disk[firstFree]<-disk[last]
        disk[last] <- -1


        compact disk

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


type Sector =
    | File of int * int
    | Free of int
    member this.Size =
        match this with 
        | File(_,s) -> s 
        | Free(s) -> s
        

let rec fill2' pos id sectors (disk: Sector[]) =
    match sectors with
    | [] -> disk
    | file :: free :: rest ->
        disk[pos] <- File(id, file)
        disk[pos+1] <- Free free
        fill2' (pos+2) (id+1) rest disk
    | [file] ->
        disk[pos] <- File(id, file)
        disk

let fill2 (sectors: int list) = 
    let disk = Array.zeroCreate (sectors.Length)
    fill2' 0 0 sectors disk
    


let merge (disk: Sector[]) =
    seq {
        let mutable last = Free 0
        for i in 0 .. disk.Length-1 do
            match last, disk[i] with
            | Free(llen), Free(len) ->
                last <- Free(llen + len)
            | Free 0, s ->
                last <- s
                
            | _, s ->
                last
                last <- s
        last
    } |> Seq.toArray


let rec compact2 id (disk2: Sector[]) =
    if id >= 0 then
        let fileIndex = disk2 |> Array.findIndex (function File(i,_) -> i = id | _ -> false)
        let file = disk2[fileIndex]

        let firstFree =
            disk2 |> Array.tryFindIndex(function
                | File _ -> false
                | Free freeLen -> freeLen >= file.Size 
            )

        let disk2 =
            match firstFree with
            | Some n when n < fileIndex ->
                let free = disk2[n]
                disk2[n] <- file
                disk2[fileIndex] <- Free(file.Size)
                let disk2 =
                    if free.Size > file.Size then
                        Array.insertAt(n+1)  (Free(free.Size - file.Size)) disk2
                    else
                        disk2


                merge disk2
            | _ -> 
                disk2
        let id = id-1
        compact2 id disk2

    else
        disk2

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


let disk2 = fill2  input |> Array.filter (fun s -> s <> Free 0) |> merge
let lastFile = disk2 |> Seq.choose(function File(id,_) -> Some id | _ -> None) |> Seq.max

compact2 lastFile disk2
|> expand
|> checksum
