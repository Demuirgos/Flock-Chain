module Block
    open FSharp.Core
    open System
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
        let encode v = System.Text.Encoding.ASCII.GetBytes(Seq.toArray v)
        let format = sprintf "%A-;%s-;%A-;%d" d.Date d.Previous d.Datum corrector
        let procedure (v:byte []) = System.Security.Cryptography.SHA512.Create().ComputeHash(v)
        format |> encode |> procedure |> BitConverter.ToString

    let rec Create idx data diff= 
        Mine {
            Index    = idx
            Data     = data
            Nonce    = 0
            Hash     = hash data 0 
        } diff

        

    and Mine b = 
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
        mine d b.Nonce
    
    let genesis = 
        let (data:Data<Contract>) = { Date = DateTime.Now; Previous = ""; Datum = None }
        {
            Index    = 0
            Data     = data
            Nonce    = 0
            Hash     = hash data 0
        }