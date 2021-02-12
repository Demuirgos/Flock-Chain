module Block
    open FSharp.Core
    open System
    open Utils
    open Contract

    type Difficulty = 
    | Nothing
    | Easy
    | Medium 
    | Hard 
    | Extreme

    type 'T Data = {
            Date     : DateTime  
            Previous : string
            Datum    : 'T Option
       }
    and 'T Block = {
            Index    : int
            Data     : 'T Data
            Nonce    : int
            Hash     : string
        }

    let hash d corrector= 
        hash (sprintf "%A-;%s-;%A-;%d" d.Date d.Previous d.Datum corrector)

    let rec create idx data diff= 
        ({  Index    = idx
            Data     = data
            Nonce    = 0
            Hash     = hash data 0 
        }, diff) ||> mine |> wrap

    and mine b= 
        let {Index = _; Nonce = n; Data = d; Hash = _} = b
        let checkPrefix (hash:string) (expected:string) = 
            (hash.StartsWith(expected), hash)
        let check diff hash = 
            checkPrefix hash (  match diff with 
                                                | Nothing ->  "" 
                                                | Easy    ->  "00"
                                                | Medium  ->  "00-00-00-00"
                                                | Hard    ->  "00-00-00-00-00-00-00-00"
                                                | Extreme ->  "00-00-00-00-00-00-00-00-00-00-00-00-00-00-00-00" )
        let rec mine data nonce diff =
            match (data, nonce) ||> hash |> check diff  with
            | (true , h)  -> {b with Nonce = nonce; Hash = h}
            | (false, _) ->
                (data, nonce + 1, diff) |||> mine   
        mine d n 

    and wrap b = 
        let hashWrap d h = 
            Utils.hash (sprintf "%A-:%s" d h)
        { 
            b with Hash = (hashWrap b.Data b.Hash)
        }

    let genesis = 
        let (data:Data<Contracts>) = { Date = DateTime.Now; Previous = ""; Datum = None }
        {
            Index    = 0
            Data     = data
            Nonce    = 0
            Hash     = hash data 0
        }