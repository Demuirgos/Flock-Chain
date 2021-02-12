module Utils
    open FSharp.Core
    open System
    let hash d = 
        let encode v = System.Text.Encoding.ASCII.GetBytes(Seq.toArray v)
        let format = d
        let procedure (v:byte []) = System.Security.Cryptography.SHA512.Create().ComputeHash(v)
        format |> encode |> procedure |> BitConverter.ToString