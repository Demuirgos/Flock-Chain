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
        [1..5] |> List.map (fun e -> 
        seq {
            for _ in 0..2 do
                yield sign (System "sender") (System "receiver") 123
        } |> Seq.toList )
    let rec combine (chain:Chain) contracts = 
        match contracts with 
        | c::cs -> 
            match transact chain c with
                   | Ok(_) as v ->   match Process (Miner "MinerId 1") v with
                                     | Ok(newChain) -> 
                                        combine newChain cs
                                     | Error(msg) -> 
                                       Error(msg)
                   | Error(msg) -> 
                        Error(msg)
        | _ -> Ok(chain)
    (aymanChain, contracts) ||> combine |> ( Process (Miner "MinerId 1") ) |> ( Process (Miner "MinerId 2") ) |>
    function
    | Ok(c) -> Ok(c, c |> validate)
    | _ -> Error("Chain invaid")
    |> printf "%A" 
    0