open System 

//target area 
let a = 241
let b = 275
let c = -75
let d = -49

//Vy(i+1) = Vyi - 1 = Vy0 - (i + 1) => Vyi = Vy0 - i, Y(i+1) = Yi + Vi  => Y(i+1) = (i+1) * 0.5 (Vy0 + Vyi) = (i+1) (Vy0 - i/2)
//Vx(j+1) = max(0, Vxj - 1)=> Vxj = max(0, Vx0 - 1), X(j+1) = (j + 1) (Vx0 - j/2 ), X(i+1) = X(j+1) where j <= i 
//max j = Vx0, j <= Vx0
// on target 
// a <= (j + 1) (Vx0 - j/2) <= b
// c <= (i + 1) (Vy0 - i/2) <= d, c <= 1/2 (i + 1) (2Vy0 - i) <= d, -2c >= (i + 1) (i - 2Vy0) >= -2d, i >= 2Vy0 and Vy0 should be maximal for maximal height
let factorize i = 
    [for j in 1..int(sqrt(float i) + 1.) do
        if i % j = 0 then yield i / j, j] 
let yvs() =
    [ for x in (-2*d)..(-2*c) do //range of (i + 1) (Vy0 - i/2)
        for t1, t2 in factorize x do 
            let i = t1 - 1 
            let _2v0 = i - t2
            if _2v0 % 2 = 0 then 
                yield i, _2v0 / 2
            let i = t2 - 1
            let _2v0 = i - t1
            if _2v0 % 2 = 0 then 
                yield i, _2v0 / 2 ] |> Set.ofList

let p1() = 
    let v = yvs() |> Seq.maxBy snd |> snd
    v * (v + 1) / 2

let xvs() =
    [ for x in 2*a..2*b do //range of (j + 1) (2Vx0 - j)
        for t1, t2 in factorize x do 
            let j = t1 - 1 
            let _2v0 = j + t2
            if _2v0 > 0 && _2v0 % 2 = 0 && 2*j <= _2v0 then 
                yield j, _2v0 / 2
            let j = t2 - 1 
            let _2v0 = j + t1
            if _2v0 > 0 && _2v0 % 2 = 0 && 2*j <= _2v0 then 
                yield j, _2v0 / 2
            ] |> Set.ofList
let p2() = 
    [ for (i, yv) in yvs() do 
        for (j, xv) in xvs() do 
            if (j = i || (i >= xv && j = xv)) then yield (xv, yv) ] |> Set.ofList