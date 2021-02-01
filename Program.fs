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
        seq {
            for i in 0..10 do
                yield Some (sign System System 123)
        } |> Seq.toList
    let rec combine (chain:Chain) contracts = 
        match contracts, chain.Blocks with 
        | ([], b::_) -> Ok(chain)
        | (c::cs, b::_) -> 
            let block = Some {Date = DateTime.Now; Datum = c; Previous=b.Hash}
            match add chain block with
                   | Ok(v) -> combine v cs
                   | Error(msg) -> 
                        Error(msg)
        | ([],_) | (_,[])  -> Error("Data missing")
    (aymanChain, contracts) ||> combine |> 
    function
    | Ok(c) -> Ok(c,validate c)
    | _ -> Error("Chain invaid")
    |> printf "%A" 
    0