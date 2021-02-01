module Contract
    open System.Text.Json
    open System
    type Adress = string
    type ID = 
        | Person of Adress
        | Miner  of Adress
        | System of string

    type Contract = {
        From    : ID
        To      : ID
        Ammount : int
    }

    let serialize contract = 
        contract |> JsonSerializer.Serialize 

    let sign source destination quantity = 
        {   From    = source
            To      = destination
            Ammount = quantity } 