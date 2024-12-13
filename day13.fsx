
#r "nuget: FParsec"
open System
open FParsec
open FParsec.Primitives

// A Machine parser using FParsec
// maybe longer that using Regex,
// but worked the 1st time

[<Struct>]
type Point = { X: int64; Y:int64}
type Machine =
    { A: Point
      B: Point
      Prize: Point}
let point x y = {X = x; Y = y}

let pPoint sep = 
    pipe2 (pchar 'X' >>. pchar sep  >>. pint64) (pstring ", Y" >>. pchar sep >>. pint64) point
let pButton c = pstring "Button " >>. pchar c >>. pstring ": " >>. pPoint '+' .>>  newline

let pPrise = pstring "Prize: " >>. pPoint '='  .>> newline

let pMachine =
    pipe3 (pButton 'A') (pButton 'B') pPrise (fun a b p -> { A = a; B= b; Prize = p} )

let pMachines = sepBy pMachine newline

let parse s = run pMachines s

let input = 
    IO.File.ReadAllText("input/day13.txt")
     |> parse
     |> function 
        | Success(s,_,_) -> s
        | Failure(f,_,_) -> failwithf "%s" f

input[input.Length-1]

/// This is simply a 2 variable equation that has an analytical solution:
/// a Ax + b Bx = Px
/// a Ay + b By = Py
/// multiply 1st line by By, 2nb by Bx
/// a AxBy + b BxBy = PxBy
/// a BxAy + b BxBy = BxPy
/// sub line2 from line 1
/// a (AxBy-BxAy) = (PxBy - BxPy)
/// a = (PxBy - BxPy) /(AxBy-BxAy)
// b is computed from a using first line

let AB m = 
    let num = m.Prize.X  * m.B.Y - m.B.X * m.Prize.Y
    let d = m.A.X * m.B.Y - m.B.X * m.A.Y
    let a = num/d
    let b = (m.Prize.X - a * m.A.X) / m.B.X

    // the solution is not necessary integers, so check that
    // no rounding happened, otherwise, there is no solution
    if a * m.A.X + b * m.B.X = m.Prize.X
        && a * m.A.Y + b * m.B.Y = m.Prize.Y then
        Some(a,b)
    else
        None 

// compute the number of tokens
let tokens (a,b) = a * 3L + b 

input
|> List.choose AB
|> List.sumBy tokens


// With an analytical solution, part 2 was trivial.
// Just changing int32 to int64 and adding 10000000000000
// the rest didn't change
// Of course any iterative solution would take forever

let (++)  p1 p2 = { X = p1.X+p2.X; Y = p1.Y+p2.Y }
let translate m =
    { m with Prize = m.Prize ++ { X = 10000000000000L; Y = 10000000000000L }}
input
|>  List.map translate
|> List.choose AB
|> List.sumBy tokens


