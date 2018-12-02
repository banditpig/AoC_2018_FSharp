open System.IO

let readLines = File.ReadLines("Day2.txt")

let asCharList (s:string) =
        [for c in s -> c]
        
let asString (chs: char list) =
    chs |> List.toArray |> System.String
    
let input = readLines
            |> Seq.map string
            |> Seq.toList
            |> List.map asCharList


let insertWith f (k:'k) (v:'v) (mp: Map<'k,'v>):Map<'k,'v> =
    if (Map.containsKey k mp) then
         let oldV = Map.find k mp
         Map.add k (f v oldV) mp
    else
         Map.add k v mp


let  threesAndTwos (mp: Map<char, int>): int * int =
    let vs = Map.toSeq mp |> Seq.map(fun (k, v) -> v)
    let threes = if Seq.contains 3 vs then 1 else 0
    let twos   = if Seq.contains 2 vs then 1 else 0
    twos, threes

let sumAndMult pairs =
    let (a, b) = List.fold (fun (x,y) (acx, acy) -> (x + acx, y + acy)) (0,0) pairs
    a * b
    
let rec countMatches (mp: Map<char, int>) (str:char list) : Map<char, int> =
    match str with
    | [] -> mp
    | x::xs  -> let mp' = insertWith (+) x 1 mp 
                countMatches mp' xs 

let evaluate xs  =
     xs
         |> List.map (countMatches Map.empty) 
         |> List.map threesAndTwos
         |> sumAndMult

   
let strDiff (xs:char list) (ys: char list)  =
    List.map2 (fun x y -> if x = y then x else '_' )  xs ys
    |> List.filter(fun c -> c <>  '_')

let rec compareCodes (x::xs)  =   
    let mapped = List.map(fun xi -> strDiff x xi) xs
    let resList = [ for s in mapped do if (List.length s = 25) then yield s]
    if (List.length resList = 1) then List.head resList
     else compareCodes xs 

     
let part1 = evaluate input
let part2 = compareCodes input

printfn "%A" part1
printfn "%A" ( asString part2 )
