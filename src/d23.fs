open System 

//think of best representation of burrow here that we need here...
//0 1 15  2 16 3 17 4 18 5 6
//    7     8    9   10
//    19   20   21   22
//    23   24   25   26
//   11    12   13   14
//amphipods of different kind
//a - 0, b - 1, c - 2, d - 3
type Burrow = { amphipods: (int*int) list; e: int; target: int list array; l: int; delayed: Set<int> }
let energies = [| 1; 10; 100; 1000 |]

let map = [| //cell connections
    [1]
    [0; 15]
    [15;16]
    [16;17]
    [17;18]
    [18; 6]
    [5]
    [15; 19]// [15;11]
    [16; 20]// [16;12]
    [17; 21]// [17;13]
    [18; 22]// [18;14]
    [23]
    [24]
    [25]
    [26] 
    [1; 7; 2]
    [2; 8; 3]
    [3; 9; 4]
    [4; 10; 5]
    [7;23]
    [8;24]
    [9;25]
    [10;26]
    [19;11]
    [20;12]
    [21;13]
    [22;14]
|]

// let burrow = 
//     { 
//         amphipods = [ (12, 0); (9, 0); (11, 1); (8, 1); (13, 2); (7, 2); (14, 3); (10, 3) ]; 
//         e = 0; target = [| [11;7]; [12;8]; [13;9]; [14;10] |]; 
//         l = 0; delayed = Set.empty 
//     }
// let burrow = 
//     { 
//         amphipods = [ (12, 0); (9, 0); (11, 1); (8, 1); (13, 2); (7, 2) ]; 
//         e = 0; target = [| [11;7]; [12;8]; [13;9]; [] |]; 
//         l = 0; delayed = Set.empty 
//     }    
let burrow = 
    { 
        amphipods = [ 
            (8, 0); (9, 1); (7, 2); (10, 3); 
            (19,3); (20,2); (21,1); (22,0); 
            (23,3); (24,1); (25,0); (26,2); 
            (11, 3); (12, 2); (13, 0); (14, 1) ]; 
        e = 0; target = [| [11;23;19;7]; [12;24;20;8]; [13;25;21;9]; [14;26;22;10] |]; 
        l = 0; delayed = Set.empty 
    }    

let sol = 
    { 
        amphipods = [ ]; 
        e = 100000; target = [| []; []; |]; 
        l = 100000; delayed = Set.empty 
    }       

let hall = Set [0..6]

let ignoreNodes = Set [15..18]

let rec reachable visited acc d occupied pos = 
    let newD = d + 1
    Array.item pos map 
    |> Seq.filter(fun p -> 
        not(Set.contains p occupied) && not(Set.contains p visited))
    |> Seq.fold(fun acc newP -> 
        // printfn "\t%A -> %A (%d)" pos newP newD
        let visited = Set.add newP visited
        let acc = if ignoreNodes.Contains newP then acc else Map.add newP newD acc 
        reachable visited acc newD occupied newP
    ) acc 

//reachable Set.empty Map.empty 0 (Set [7;8;9;10;11;12;13;14]) 8

let reachableM =
    let mutable dists = Map.empty 
    fun occupied pos -> 
        match Map.tryFind (occupied, pos) dists with 
        | Some d -> d 
        | None -> 
            let d = reachable Set.empty Map.empty 0 occupied pos 
            dists <- Map.add (occupied, pos) d dists 
            d 

//reachableM Set.empty poss 11

let rec insertSorted prefix p =
    function
    | p1::suffix when (List.length p1.amphipods, p1.e) < (List.length p.amphipods, p.e) -> insertSorted (p1::prefix) p suffix
    | suffix -> prefix |> List.fold(fun acc p -> p::acc) (p::suffix)

let rec move n solution poss = //invariant - targets are precomputed to possible targets only
    if n = 0 then 
        poss |> List.iter(printfn "%A")
        solution 
    else 
        let newN = if n = -1 then n else n - 1
        match poss with
        | [] -> solution
        | ({amphipods = []; e = e; l = l} as s)::otherPos ->         
            let newSolution = 
                if (e, l) < (solution.e, solution.l) then 
                    printfn "Solution: %A" s
                    s 
                else solution
            move newN newSolution (otherPos |> List.filter(fun b -> b.e < newSolution.e))    
        | ({amphipods = (p, i)::otherA} as burrow)::otherPos -> 
            let occupied = burrow.amphipods |> Seq.map fst |> Set.ofSeq //posToPoss pos         
            // printfn "Trying %A" (p, i) 
            let targetI = burrow.target.[i]
            match targetI with 
            | [] -> failwithf "Inconsistency - we have amphipods but no targets for them: %A, %d" burrow.amphipods i
            | t::otherT when p = t -> //on target
                // printfn "On target %A" (p, i)
                let newT = burrow.target.[0..]
                newT.[i] <- otherT 
                let newBurrow = {burrow with amphipods = otherA; target = newT }
                move newN solution (newBurrow::otherPos)            
            | t::otherT -> 
                let r = reachableM occupied p    
                // printfn "Target %A %A %A" (p, i) t r
                match Map.tryFind t r with 
                | Some dist -> 
                    // printfn "Move to target %A -> %A" p t
                    let newT = burrow.target.[0..]
                    newT.[i] <- otherT 
                    let newBurrow = 
                        {amphipods = otherA; e = burrow.e + dist * energies.[i]; 
                            target = newT; l = burrow.l + 1; delayed= Set.empty}
                    let newPos = if newBurrow.e >= solution.e then otherPos else insertSorted [] newBurrow otherPos
                    move newN solution newPos
                | None when Set.contains p hall -> //cannot move from hall to hall                        
                    let delayedA, possibleA = otherA |> List.partition(fst >> burrow.delayed.Contains)
                    match possibleA with 
                    | [] -> //tried all but deadlock - throw away pos
                        // printfn "Deadlock %A" (p, i)
                        move newN solution otherPos 
                    | ah::at -> 
                        // printfn "Delay %A" (p, i)
                        let newA = (ah::(p, i)::delayedA) @ at
                        let newBurrow = {burrow with amphipods = newA; delayed = Set.add p burrow.delayed}
                        move newN solution (newBurrow::otherPos)
                | None -> //move to hall - all possibilities                                
                    let newPos = 
                        r
                        |> Map.toSeq
                        |> Seq.filter(fun (id, _) -> hall.Contains id) 
                        |> Seq.fold(fun acc (h, dist) -> 
                            // printfn "\tMove %A --> %A" p h
                            let delayedA, possibleA = otherA |> List.partition(fst >> burrow.delayed.Contains)
                            let newA = delayedA @ ((h, i)::possibleA)
                            let newBurrow = {burrow with amphipods = newA; delayed = Set.empty; l = burrow.l + 1; e = burrow.e + dist * energies.[i]}
                            let acc = if newBurrow.e >= solution.e then acc else insertSorted [] newBurrow acc
                            acc
                        ) otherPos
                    //+ delayed current
                    let delayedA, possibleA = otherA |> List.partition(fst >> burrow.delayed.Contains)
                    match possibleA with 
                    | [] -> //tried all but deadlock - throw away pos
                        // printfn "Deadlock %A" (p, i)
                        move newN solution newPos 
                    | ah::at -> 
                        // printfn "Delay %A" (p, i)
                        let newA = (ah::(p, i)::delayedA) @ at
                        let newBurrow = {burrow with amphipods = newA; delayed = Set.add p burrow.delayed}
                        move newN solution (newBurrow::newPos)

// move -1 sol [burrow]

// let s, pos2 = move ([], []) pos 

// pos2 |> List.iter (printfn "%A")

// let s, pos3 = pos2 |> List.fold(move) (s, [])

// pos3 |> List.iter (printfn "%A")

// let s, pos4 = pos3 |> List.fold(move) (s, [])

// pos4 |> List.iter (printfn "%A")

// let rec game acc = 
//     function 
//     | [] -> acc 
//     | poss -> poss |> List.fold(move) (acc, []) ||> game

// let rss = game [] [pos]    

// List.length rss
// rss |> List.map(fun a -> a.e) |> List.min
// rss |> List.map(fun a -> a.e) |> List.max
// rss |> List.sortBy(fun a -> a.e) |> List.take 10 |> List.iter(fun a -> printfn "%A %A" a.e (List.length a.h))