module Days.Day3
open System.Text.RegularExpressions
open System.IO

let readLines = File.ReadLines("/Users/mikehoughton/AoC_2018_FSharp/Days/Day3.txt")
let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

type Point = Point of int * int
// Left x, y Right  x,y
type Rectangle =  {Lx:int; Ly:int; Rx:int; Ry:int}

let makeRect x y w h : Rectangle = {Lx = x; Ly = y; Rx = x + w; Ry = y + h} 
let min x y = 
    match x > y with
        | true -> y
        | _    -> x 

let max x y = 
    match x > y with 
        | true -> x
        | _    -> y

let overlapArea r1 r2 = 
   let {Lx = lx1; Ly = ly1; Rx = rx1; Ry = ry1} = r1
   let {Lx = lx2; Ly = ly2; Rx = rx2; Ry = ry2} = r2

   let area = (min rx1 rx2 - max lx1 lx2)
                * 
              (min ry1 ry2 - max ly1 ly2)
   area


let rect xydata  =
    match xydata with
    | Regex @"#\d+ @ (\d+),(\d+): (\d+)x(\d+)" [ x1;y1;w;h ] ->
        Some (makeRect (int x1) (int y1) (int w) (int h))
    | _ -> None


let allRects = 
    readLines |> Seq.map rect
