open System
open System.IO 

let scanners = 
    File.ReadAllText("../input/d19").Split("\n\n", StringSplitOptions.RemoveEmptyEntries) 
    |> Seq.map(fun (x: string) -> 
        x.Split("\n", StringSplitOptions.RemoveEmptyEntries) |> Seq.tail 
        |> Seq.map(fun (x: string) ->
            x.Split(",") |> Seq.map int |> Seq.toList)
        |> Seq.toList) 
    |> Seq.toArray

let pointF f a b = Seq.zip a b |> Seq.map(fun (x, y) -> f x y) |> Seq.toList
let (.+) a b = pointF (+) a b
let (.-) a b = pointF (-) a b
let (.|-|) a b = pointF (fun a b -> abs(a - b)) a b
let (.*) a b = pointF (*) a b
let (.=) a b = pointF (=) a b |> List.forall id

let permutations a = 
    let rec perms acc = 
        function 
        | [] -> acc 
        | el::other -> 
            let acc = 
                [ for l in acc do 
                    yield! [ for i in 0..List.length l -> 
                                List.take i l @ (el::List.skip i l) ] ]
            perms acc other
    in perms [[]] a

let combineDigits a = 
    let rec combineDigitsI acc =
        function 
        | [] -> acc 
        | digits::other -> 
            let acc = [ for l in acc do for d in digits -> d::l ]
            combineDigitsI acc other
    in combineDigitsI [[]] a

let toSys (axis, orients) = List.permute(fun i -> List.item i axis) >> (.*) orients

let findCommonBeacons s1 s2 = 
    let dims = List.length(List.head s1) //assuming at least one beacon here
    seq {
        for axis in permutations [0..dims - 1] do 
            for orients in combineDigits [for i in 0..dims - 1 -> [1;-1]] do //last axis orient is fixed to others              
                let vnOpt = 
                    seq { for beacon2 in s2 do 
                            let shift = toSys (axis, orients) beacon2
                            for beacon1 in s1 -> beacon1 .- shift
                    } |> Seq.groupBy id |> Seq.map(fun (key, s) -> key, Seq.length s) |> Seq.filter (snd >> ((<=) 12)) 
                    |> Seq.tryHead
                    // |> Seq.map(fun (v, n) -> (v, n, (axis, orients))) |> Seq.toList
                match vnOpt with 
                | Some (v, n) -> yield v, n, (axis, orients)           
                | _ -> ()
    } |> Seq.tryHead

let findScannerRelPos scanners = 
    [ for i in 0..Array.length scanners - 1 do 
        for j in i+1..Array.length scanners - 1 do 
            match findCommonBeacons scanners.[i] scanners.[j] with 
            | Some (v, num, (axis, orients)) -> 
                yield (i, j), (v, num, (axis, orients))
                let axisRev = [0..List.length axis - 1] |> List.permute(fun i -> List.item i axis)
                let orientsRev = orients |> List.mapi(fun i _ -> List.item (List.item i axis) orients)
                let minusOne = [for i in 0..List.length axis - 1 -> -1]
                yield (j, i), ((toSys (axisRev, orientsRev) v) .* minusOne, num, (axisRev, orientsRev))
            | _ -> ()
    ] |> List.groupBy (fun ((i, _), _) -> i) |> Map

let findScannerAbsPos i scannerRelMap = 
    let rec findScannerAbsPosI visited i scannerRelMap = 
        [ 
            for (_, j), (v1, _, axisOrients) in Map.find i scannerRelMap do 
                if Set.contains j visited |> not then 
                    yield j, v1
                    let absPoss = findScannerAbsPosI (Set.add j visited) j scannerRelMap
                    yield! absPoss |> List.map(fun (k, v2) -> k, v1 .+ toSys axisOrients v2)
        ]
    in findScannerAbsPosI (Set.singleton i) i scannerRelMap

let rec findBeaconsAbsPos i scannerRelMap scanners =
    let rec findBeaconsAbsPosI visited i scannerRelMap = 
        [ 
            for beacon in Array.item i scanners do 
                yield beacon
            for (_, j), (v1, _, axisOrients) in Map.find i scannerRelMap do 
                if Set.contains j visited |> not then 
                    let absPoss = findBeaconsAbsPosI (Set.add j visited) j scannerRelMap
                    yield! absPoss |> Set.map(fun v2 -> v1 .+ toSys axisOrients v2)
        ] |> Set.ofList
    in findBeaconsAbsPosI (Set.singleton i) i scannerRelMap

let p1() = 
    let beacons = findBeaconsAbsPos 0 (findScannerRelPos scanners) scanners
    Set.count beacons //359
    // beacons |> Set.iter(fun [x; y; z] -> printfn "%A,%A,%A" x y z)

let p2() = 
    let scanners = findScannerAbsPos 0 (findScannerRelPos scanners)
    let poss = [0; 0; 0]::(scanners |> List.map snd) |> List.toArray
    [
        for i in 0..Array.length poss - 1 do 
            for j in i + 1 .. Array.length poss - 1 do 
                yield poss.[i] .|-| poss.[j] |> List.sum
    ] |> List.max