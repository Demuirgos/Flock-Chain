module Client
    open System.Net.Sockets

    type Client<'t>(user:'t,server, port) as c = 
        inherit TcpClient(server, port)
        member val User = user with get
        member c.Send(msg) =
            let data = System.Text.Encoding.ASCII.GetBytes(Seq.toArray msg)
            use stream = c.GetStream()
            stream.AsyncWrite data |> Async.RunSynchronously
        member c.Listen() = 
            let result = async  {   use stream = c.GetStream()
                                    let! data = stream.AsyncRead(256)
                                    return data
                                } |> Async.RunSynchronously
            result
        member c.Connect(msg) = 
            c.Send(msg)
            c.Listen() |> printf "%A"
        

