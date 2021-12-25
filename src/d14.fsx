open System
open System.IO

let input = File.ReadAllText("d14")

let [| polymerS; rulesS |] = input.Split("\n\n", StringSplitOptions.RemoveEmptyEntries)
let polymer = polymerS |> Seq.toList
let rules = rulesS.Split("\n") |> Array.map(fun s -> 
    let sp = s.Split(" -> ")
    ((sp.[0].[0], sp.[0].[1]), sp.[1].[0])) |> Map.ofArray

let runStep = 
    let rec runStepI acc = 
        function 
        | ch1::ch2::tail -> 
            match Map.tryFind (ch1, ch2) rules with 
            | None -> runStepI (ch1::acc) (ch2::tail)
            | Some insertion -> runStepI (insertion::ch1::acc) (ch2::tail)
        | ch::[] -> ch::acc |> List.rev
        | [] -> []
    in runStepI []

let rec runNSteps n f = 
    if n = 0 then id 
    else f >> runNSteps (n-1) f
    
let p1() = 
    let res = runNSteps 10 runStep polymer
    let counts = res |> List.countBy id 
    (counts |> List.maxBy snd |> snd) - (counts |> List.minBy snd |> snd)

let patterns = 
    let rec convertToPatterns (patterns, counts) = 
        function 
        | ch1::ch2::tail when Map.containsKey (ch1, ch2) rules -> 
            let patterns = Map.change (ch1, ch2) ((function | None -> 1UL | Some cnt -> cnt + 1UL) >> Some) patterns
            let counts = Map.change ch1 ((function | None -> 1UL | Some cnt -> cnt + 1UL) >> Some) counts
            convertToPatterns (patterns, counts) (ch2::tail)
        | ch1::[] -> 
            let counts = Map.change ch1 ((function | None -> 1UL | Some cnt -> cnt + 1UL) >> Some) counts
            patterns, counts
        | _ -> patterns, counts
    in convertToPatterns (Map.empty, Map.empty)

let ruleUpdaters = 
    rules |> Map.map(fun (ch1, ch2) insertion -> 
        let f1 = 
            if Map.containsKey (ch1, insertion) rules then 
                fun n -> Map.change (ch1, insertion) ((function Some cnt -> cnt + n | _ -> n) >> Some)                
            else fun n -> id        
        let f2 = 
            if Map.containsKey (insertion, ch2) rules then 
                fun n -> Map.change (insertion, ch2) ((function Some cnt -> cnt + n | _ -> n) >> Some)
            else fun n -> id
        let patternUpdater = fun n -> 
            Map.change (ch1, ch2) (function Some cnt when cnt > n -> Some (cnt - n) | _ -> None)
            >> f1 n >> f2 n
        let countUpdater = fun n -> 
            // printfn "Updating count of %A by %A cnt %A" insertion (ch1, ch2) n
            Map.change insertion ((function Some cnt -> cnt + n | _ -> n) >> Some)
        patternUpdater, countUpdater)

let polymerPatterns = patterns polymer
let runStep2 (pp, pc) = 
    // printfn "-----"
    pp |> Map.fold(fun (accP, accC) key n -> 
        let pUpd, cUpd = ruleUpdaters.[key]
        let accP = pUpd n accP
        let accC = cUpd n accC
        accP, accC) (pp, pc)
let p2 k = 
    let _, res = runNSteps k runStep2 polymerPatterns
    let counts = res |> Map.toList
    (counts |> List.maxBy snd |> snd) - (counts |> List.minBy snd |> snd)            