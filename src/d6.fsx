open System.IO

let firstGeneration = File.ReadAllText("d6").Split(",") |> Array.map int  

let reproductionCicle = 7
let rec createFishFromCicleEnds(startAge) = 
    seq { yield startAge; yield! createFishFromCicleEnds (startAge + reproductionCicle) }

let createFish maxAge startAge = createFishFromCicleEnds(startAge) |> Seq.map((+) 1) |> Seq.takeWhile((>=) maxAge) //birth goes on next day

//other approach - birthday cache
let maxDay = 256
let fishes: int list array = [| for i in 0..maxDay -> [] |]
do for i in 1..(maxDay-1) do
        let fish = createFish maxDay i 
        let firstChild = Seq.head fish 
        fishes.[firstChild] <- Seq.toList fish

let childGrowth = 8
let rec addBirthes (birthes: uint64 option array) fishId = 
    match birthes.[fishId] with 
    | Some cnt -> cnt 
    | None ->
        let children = fishes.[fishId]
        //identifies fish kind 
        let mutable cnt = uint64 (List.length children)
        for child in children do
            let childId = child + childGrowth + 1
            if childId < Array.length fishes then 
                cnt <- cnt + addBirthes birthes childId
        birthes.[fishId] <- Some cnt
        cnt

let p2() = 
    let birthes: uint64 option array = [| for i in 0..maxDay -> None |]
    let g1Count = uint64(Array.length(firstGeneration))
    let otherCount = 
        Array.countBy id firstGeneration
        |> Array.sumBy(fun (fishId, count) ->
            let bths = addBirthes birthes (fishId + 1)
            bths * uint64(count))
    g1Count + otherCount
