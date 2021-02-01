module Contract
    open System.Text.Json
    open System
    type ID = 
        | Person of string
        | System

    type Contract = {
        From    : ID
        To      : ID
        Ammount : int
    }

    let serialize contract = 
        contract |> JsonSerializer.Serialize 

    let sign source destination quantity = 
        {   From    = source;
            To      = destination;
            Ammount = quantity; } 