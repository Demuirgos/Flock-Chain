// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Chain
open Block
open Contract
// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let aymanChain = Difficulty |> Chain.Genesis 
    let contracts = 
        [1..10] |> List.map (fun e -> 
        seq {
            for _ in 0..10 do
                yield sign (System "sender") (System "receiver") 123
        } |> Seq.toList )
    let rec combine (chain:Chain) contracts = 
        match contracts with 
        | c::cs -> 
            match transact chain c with
                   | Ok(v) -> combine v cs
                   | Error(msg) -> 
                        Error(msg)
        | _ -> Ok(chain)
    (aymanChain, contracts) ||> combine |> ( Process (Miner "MinerId") ) |>
    function
    | Ok(c) -> Ok(c, c |> validate)
    | _ -> Error("Chain invaid")
    |> printf "%A" 
    0