open System 

let rolledNum, dDice = 
    let mutable state = 1 
    let mutable rolled = 0
    (fun () -> rolled), fun () -> 
        rolled <- rolled + 1
        let prev = state          
        state <- if prev = 100 then 1 else prev + 1
        prev        

let pawn start = 
    let mutable state = start 
    fun (inc) -> 
        state <- (state + inc - 1) % 10 + 1
        state

let game maxScore dice pawn1 pawn2 = 
    let mutable score1 = 0
    let mutable score2 = 0
    let mutable step = 0
    fun () -> 
        step <- step + 1
        let points = pawn1 (dice() + dice() + dice())            
        score1 <- score1 + points
        if score1 >= maxScore then 
            Some (step, score2)
        else 
            let points = pawn2 (dice() + dice() + dice())            
            score2 <- score2 + points
            if score2 >= maxScore then 
                Some (step, score1)
            else None

let p1 start1 start2 = 
    let g = game 1000 dDice (pawn start1) (pawn start2)
    let rec playGame() = 
        match g() with 
        | None -> playGame()
        | Some (_, s) -> rolledNum() * s
    playGame()

let combineDigits a = 
    let rec combineDigitsI acc =
        function 
        | [] -> acc 
        | digits::other -> 
            let acc = [ for l in acc do for d in digits -> d::l ]
            combineDigitsI acc other
    in combineDigitsI [[]] a

let pointsDistribution = 
    combineDigits [[1;2;3];[1;2;3];[1;2;3]]
    |> List.groupBy(List.sum) |> List.map(fun (points, cases) -> (points, List.length cases |> uint64))

let rec qStep acc pawnDistribution =  
    match pawnDistribution with 
    | [] -> acc |> List.rev
    | _ ->        
        // printfn "%A" pawnDistribution
        let newPawnDistribution = 
            [ for (pawn, pwnScoreDistr) in pawnDistribution do 
                for (points, pChances) in pointsDistribution do 
                    let newPoints = (pawn + points - 1) % 10 + 1 
                    yield newPoints, [for (score, sChances) in pwnScoreDistr ->
                        score + newPoints, pChances * sChances]
                    ]
        // printfn "%A" newPawnDistribution
        let wins, allOther, pawnDist = 
            newPawnDistribution |> List.fold(fun (allWins, allOther, pawnDist) (pawn, scoreDistr) -> 
                let wins, other = scoreDistr |> List.partition (fst >> (<=) 21)
                let pawnDist = 
                    match other with 
                    | [] -> pawnDist
                    | _ -> (pawn, other)::pawnDist
                let wins = (wins |> List.sumBy snd) + allWins
                let allOther = (other |> List.sumBy snd) + allOther
                wins, allOther, pawnDist) (0UL, 0UL, [])
        // printfn "--> %A" wins
        // printfn "%A" pawnDist
        let acc = (wins, allOther)::acc 
        qStep acc pawnDist

let m1 = (qStep [] [1,[0,1UL]]) @ [0UL,0UL]
let m2 = qStep [] [6,[0,1UL]]      
List.zip m1 m2
let rec combineP1P2 acc =  
    function 
    | [], [] -> acc |> List.rev 
    | p1::o1, p2::o2 -> combineP1P2 (p2::p1::acc) (o1, o2)

let rec sumChancesP1 (acc: uint64) = 
    function 
    | (_, l2)::(w1,_)::other -> sumChancesP1 (l2 * w1 + acc) other
    | [] -> acc 

sumChancesP1 0UL ((0UL, 1UL)::(combineP1P2 [] (m1, m2)) @ [0UL, 0UL])    

let rec sumChancesP2 (acc: uint64) = 
    function 
    | (_, l1)::(w2,_)::other -> sumChancesP2 (l1 * w2 + acc) other
    | [] -> acc 

sumChancesP2 0UL (combineP1P2 [] (m1, m2))    
