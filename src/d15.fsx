open System
open System.IO

type Cell = { i:int; j: int; x: int; p: int option  }
let aInts = 
    File.ReadAllLines("d15") |> Array.map(Seq.map (string >> int) >> Seq.toArray)

let joinArraysByX (arrs: _ list) = 
    [| for i in 0..(Array.length (arrs.[0]) - 1) ->  Array.concat(arrs |> List.map(fun arr -> arr.[i])) |]
let buildCells nTimes aInts = 
    [ 
        for i in 0..(nTimes-1) ->
            joinArraysByX [
                for j in 0..(nTimes-1) -> 
                    let delta = i + j
                    aInts |> Array.map(Array.map(fun x -> (x + delta - 1) % 9 + 1))
            ]
    ] |> Array.concat

let a = buildCells 5 aInts |> Array.mapi(fun i -> Array.mapi(fun j x -> { i = i; j = j; x = x; p = None }))
a.[0].[0] <- {a.[0].[0] with p = Some 0}

let rec buildPathes =
    function 
    | [] -> ()
    | ({i = i; j = j; p = Some p}::_ as shortest)::otherPathes -> 
        // printfn "> expand at (%A, %A), %A" i j p
        let buildPath newI newJ = 
            match a.[newI].[newJ] with 
            | { p = None; x = x } -> 
                let newP = p + x
                a.[newI].[newJ] <- {a.[newI].[newJ] with p = Some newP}
                // printfn "\t value at (%A, %A) is %A" newI newJ newP
                [a.[newI].[newJ]::shortest]
            | _ -> []
        let newPathes = 
            [
                let iUp = i - 1 in if iUp >= 0 then yield! buildPath iUp j
                let iDown = i + 1 in if iDown < a.Length then yield! buildPath iDown j 
                let jLeft = j - 1 in if jLeft >= 0 then yield! buildPath i jLeft 
                let jRight = j + 1 in if jRight < a.[i].Length then yield! buildPath i jRight
            ]
        let rec insertSorted prefix p path pathes = 
            match pathes with 
            | [] -> path::prefix |> List.rev 
            | ({p = Some p2}::_)::_ when p2 >= p ->
                (path::prefix) |> List.fold(fun acc pth -> pth::acc) pathes
            | path2::otherPathes -> insertSorted (path2::prefix) p path otherPathes

        //inserting new pathes to sorted 
        newPathes |> List.fold(fun acc (({p = Some p}::_) as path) -> insertSorted [] p path acc) otherPathes
        |> List.filter(fun ({i = i; j = j}::_) -> 
            ((i - 1 >= 0) && (Option.isNone a.[i-1].[j].p)) ||
            ((i + 1 < a.Length) && (Option.isNone a.[i+1].[j].p)) ||
            ((j - 1 >= 0) && (Option.isNone a.[i].[j-1].p)) ||
            ((j + 1 < a.[i].Length) && (Option.isNone a.[i].[j+1].p)))
        |> buildPathes

buildPathes [[a.[0].[0]]]

let p = a.[a.Length-1].[a.[a.Length-1].Length-1]