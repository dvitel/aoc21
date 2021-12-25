open System
open System.IO

exception ParseError of msg:string*bits:int list*pos:int;

let bitsToInt: seq<int> -> int = Seq.rev >> Seq.mapi(fun i b -> (i, b)) >> Seq.fold(fun acc (i, b) -> (b <<< i) ||| acc) 0
let bitsToByte = bitsToInt >> byte
let bitsToBytes = Seq.rev >> Seq.chunkBySize 8 >> Seq.map (Seq.rev >> bitsToByte)

let rec take acc n message = 
    if n = 0 then 
        Some (List.rev acc, message)
    else 
        match message with 
        | b::message -> take (b::acc) (n-1) message 
        | _ -> None

type OperatorPacket = { operator: int; args: Packet list }
and PacketType = 
    | Literal of num: bigint
    | Operator of OperatorPacket
and Packet = { version: int; packet: PacketType }

let rec parsePacket padded pos message = 
    let version, pos, message  = parseVersion pos message 
    let packetType, pos, message = parseType pos message 
    let packet, pos, message = 
        match packetType with 
        | 4 -> 
            let num, pos, message = parseNumber [] pos message 
            { version = version; packet = Literal num }, pos, message
        | v -> 
            let operator, pos, message = parseOperator v pos message 
            { version = version; packet = Operator operator}, pos, message
    // let pos, message = parseZeroes pos message //check message completenesss
    let pos, message = 
        if padded then parsePadding pos message 
        else pos, message 
    packet, pos, message
and parsePadding pos message = 
    let skipToPos = ((pos - 1) / 4 + 1) * 4
    let delta = skipToPos - pos        
    let message = 
        match take [] delta message with 
        | Some (_, message) -> message 
        | _ -> raise (ParseError (sprintf "cannot skip zeroes till %A" skipToPos, message, pos))
    skipToPos, message
and parseVersion pos = 
    function 
    | b1::b2::b3::message -> bitsToInt [b1; b2; b3], pos + 3, message 
    | message -> raise (ParseError ("cannot parse version", message, pos))
and parseType pos = 
    function 
    | b1::b2::b3::message -> bitsToInt [b1; b2; b3], pos + 3, message 
    | message -> raise (ParseError ("cannot parse type", message, pos))
and parseNumber acc pos = 
    function 
    | 1::b1::b2::b3::b4::message -> parseNumber (b4::b3::b2::b1::acc) (pos + 5) message
    | 0::b1::b2::b3::b4::message -> 
        let acc = (b4::b3::b2::b1::acc) |> List.rev
        let num = 0::acc |> bitsToBytes |> Seq.toArray |> bigint
        let pos = pos + 5
        num, pos, message 
    | message -> raise (ParseError ("cannot parse literal", message, pos))
and parseOperator opCode pos = 
    function 
    | 0::message -> 
        let pos = pos + 1
        let bitLen, pos, message = 
            match take [] 15 message with 
            | Some (subpacketsLength, message) -> 
                bitsToInt subpacketsLength, pos + 15, message
            | _ -> raise (ParseError ("cannot parse subpackets length", message, pos))
        let finalPos = pos + bitLen
        let rec readPackets packets pos message = 
            if pos = finalPos then 
                List.rev packets, pos, message 
            else 
                let packet, pos, message = parsePacket false pos message 
                readPackets (packet::packets) pos message 
        let packets, pos, message = readPackets [] pos message                 
        {operator = opCode; args = packets }, pos, message
    | 1::message -> 
        let pos = pos + 1
        let countLen, pos, message = 
            match take [] 11 message with 
            | Some (subpacketsLength, message) -> 
                bitsToInt subpacketsLength, pos + 11, message
            | _ -> raise (ParseError ("cannot parse subpackets number", message, pos))
        let rec readPackets n packets pos message = 
            if n = 0 then 
                List.rev packets, pos, message 
            else 
                let packet, pos, message = parsePacket false pos message 
                readPackets (n-1) (packet::packets) pos message 
        let packets, pos, message = readPackets countLen [] pos message
        {operator = opCode; args = packets }, pos, message            
    | message -> raise (ParseError ("cannot parse operator, missing length", message, pos))

let rec versionSum acc = 
    function 
    | { version = v; packet = Operator { args = packets }} -> packets |> List.fold versionSum (acc + v)
    | { version = v} -> acc + v     

let p1 message = 
    try 
        parsePacket true 0 message 
        |> fun (p, _, _) -> p |> versionSum 0 
    with e -> 
        printfn "%A" e
        reraise()

let rec eval = 
    function 
    | { packet = Literal v } -> v
    | { packet = Operator { operator = 0; args = args }} -> args |> Seq.map eval |> Seq.fold (+) bigint.Zero
    | { packet = Operator { operator = 1; args = args }} -> args |> Seq.map eval |> Seq.fold (*) bigint.One
    | { packet = Operator { operator = 2; args = args }} -> args |> Seq.map eval |> Seq.min
    | { packet = Operator { operator = 3; args = args }} -> args |> Seq.map eval |> Seq.max
    | { packet = Operator { operator = 5; args = [arg1; arg2] }} -> 
        if eval arg1 > eval arg2 then bigint.One else bigint.Zero
    | { packet = Operator { operator = 6; args = [arg1; arg2] }} -> 
        if eval arg1 < eval arg2 then bigint.One else bigint.Zero
    | { packet = Operator { operator = 7; args = [arg1; arg2] }} -> 
        if eval arg1 = eval arg2 then bigint.One else bigint.Zero   

let rec pprint = 
    function 
    | { packet = Literal v } -> sprintf "%AI" v
    | { packet = Operator { operator = 0; args = args }} -> String.Join(" + ", args |> Seq.map pprint) |> sprintf "(%s)"
    | { packet = Operator { operator = 1; args = args }} -> String.Join(" * ", args |> Seq.map pprint) |> sprintf "(%s)"
    | { packet = Operator { operator = 2; args = args }} -> String.Join("; ", args |> Seq.map pprint) |> sprintf "Seq.min([%s])"
    | { packet = Operator { operator = 3; args = args }} -> String.Join("; ", args |> Seq.map pprint) |> sprintf "Seq.max([%s])"
    | { packet = Operator { operator = 5; args = [arg1; arg2] }} -> sprintf "(if (%s > %s) then 1I else 0I)" (pprint arg1) (pprint arg2)
    | { packet = Operator { operator = 6; args = [arg1; arg2] }} -> sprintf "(if (%s < %s) then 1I else 0I)" (pprint arg1) (pprint arg2)
    | { packet = Operator { operator = 7; args = [arg1; arg2] }} -> sprintf "(if (%s = %s) then 1I else 0I)" (pprint arg1) (pprint arg2)
        
let message = 
    Convert.FromHexString(File.ReadAllText("d16")) |> Array.map(fun byte -> Convert.ToString(byte, 2).PadLeft(8, '0')) 
    |> fun x -> String.Join("", x) 
    // "00111000000000000110111101000101001010010001001000000000"
    |> Seq.map (string >> int) |> Seq.toList
            
let p2 message = 
    parsePacket true 0 message 
    |> fun (p, _, _) -> p |> eval