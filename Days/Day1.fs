module Days.Day1


open System.IO

let readLines = File.ReadLines("Day1.txt")

// Part 1 ------------------------
let input = readLines |> Seq.map int
let answer1 = input |> Seq.sum

// Part 2 ------------------------
let itemAt ix (arr : array<'a>) = arr.[ix % Array.length arr]

let nextTotal total ix arry =
    match ix with
    | 0 -> itemAt ix arry
    | _ -> total + itemAt ix arry



let rec search (ix : int) (total : int) (seenMap : Map<int, int>) list =
    let nextVal = nextTotal total ix list
    match seenMap.ContainsKey nextVal with
    | true  -> nextVal
    | false -> search (ix + 1) nextVal (seenMap.Add(nextVal, nextVal)) list


let answer2 =
    search 0 0 (Map.ofList []) (input |> Seq.toArray)

// ------------------------
printfn "%A" answer1
printfn "%A" answer2