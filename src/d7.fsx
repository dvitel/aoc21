open System.IO

let poss = File.ReadAllText("d7").Split(",") |> Array.map int

// p1: sum(abs(x - xi)) --> min 
// p1': sum(sign(x - xi)) = 0 ==> num of x1 above x should be == num of xi bellow x ==> median

Array.sortInPlace poss

let possMedian = 
    let center = Array.length poss / 2
    if Array.length poss % 2 = 0 then
            (poss.[center] + poss.[center - 1]) / 2
    else poss.[center]

let p1() = poss |> Array.map((-) possMedian >> abs) |> Array.sum

// p2: sum(sum_{i=1}^{abs(x-xi)}i) --> min
// p2: sum(abs(x-xi) * (abs(x-xi) + 1) / 2) --> min
// p2: sum((x-xi)^2 + abs(x-xi)) --> min
// p2': sum(2(x-xi) + sign(x-xi)) = 0, sum(x)-sum(xi) = - sum(sign(x-xi)) / 2
// p2': x = sum(xi) / k - sum(sing(x-xi)) / 2k, x = mean - avg(sign) / 2, x ~ mean - we check ints around mean

let possMean = Array.averageBy float (poss)

let p2() = 
    seq { for mean in int (floor (possMean - 0.5))..int(ceil(possMean + 0.5)) ->
                mean, poss |> Array.sumBy (fun pos -> abs(pos - mean) * (abs(pos - mean) + 1) / 2) }
    |> Seq.minBy snd