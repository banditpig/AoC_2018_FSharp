module Day3
    
    open System
    open System.Text.RegularExpressions
    open System.IO
    open System.Drawing
    
    // Point in Drawing is no good for going into Set.
    type SimplePoint = int * int  
    let makePoint x y = (x, y)  
     
    let pairUp (a: 'A) (lsta: 'A list) = [for x in lsta do yield (a, x)] 
    
    let listPairings (lst: 'A list): ('A * 'A) list = 
       let rec build l total = 
            match l with
            | []        -> total 
            | [item]    -> total 
            | (h::rest) -> build rest (List.append total (pairUp h rest) )
        
       build lst []    


    let readLines = seq { yield!  File.ReadLines("/Users/mikehoughton/AoC_2018_FSharp/AoC/AoC/Day3.txt")}
    
    let (|Regex|_|) pattern input =
            let m = Regex.Match(input, pattern)
            if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
            else None
    
        
    let makeRect x y w h : Rectangle = Rectangle(x, y, w, h)

    let recPoints (rect: Rectangle) : SimplePoint list =
        let xr = [rect.X .. rect.Width  + rect.X - 1]
        let yr = [rect.Y .. rect.Height + rect.Y - 1]
        [for x in xr do for y in yr -> makePoint x y]
        
   
     
    let rectsIntersect (r1: Rectangle) (r2: Rectangle): Boolean = r1.IntersectsWith(r2) 
        
    let intersection (r1: Rectangle) (r2: Rectangle): Rectangle   = Rectangle.Intersect(r1, r2)
        
    let rect xydata: Rectangle option  =
        match xydata with
         | Regex @"#\d+ @ (\d+),(\d+): (\d+)x(\d+)" [ x1;y1;w;h ] ->
                  Some (makeRect (int x1) (int y1) (int w) (int h))
         | _ -> None

     
    
               
                  
    let part1 =
        
        readLines
           |> Seq.map rect
           |> Seq.choose id
           |> Seq.toList           
           |> listPairings
           |> List.filter(fun (r1, r2)  -> rectsIntersect r1 r2) 
           |> List.map(fun (r1, r2) -> intersection r1 r2)
           |> List.map(fun r -> recPoints r)
           |> List.concat
           |> Set.ofList
           |> Set.count
                          
    
    
    let part2 =
            let allRect = 
              readLines
                 |> Seq.map rect
                 |> Seq.choose id
                 |> Seq.toList
                                                                 
            let interSected  =
              allRect          
               |> listPairings
               |> List.filter(fun (r1, r2)  ->  (rectsIntersect r1 r2) = true)
               |> List.fold(fun acc (r1, r2)   -> r1::r2::acc) []
               
            let diff = 
              allRect |> List.filter(fun r -> not (List.contains r interSected))
              
            diff 
     
    let dump = 
      
     part2 |> Seq.iter (fun x -> printfn "%A " x)
                
