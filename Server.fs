module Server
    open System.Net.Sockets

    type Listener<'t>(server, port, handle) as l = 
        inherit TcpListener(server, port)
        member l.Listen() = 
            use client = l.AcceptTcpClient();
            use stream = client.GetStream();
            let loop () =
                async  {   
                        let! g = stream.AsyncRead(256)
                        let data = System.Text.Encoding.ASCII.GetString(g, 0, Array.length g)
                        let handleResult = handle data
                        stream.AsyncWrite(handleResult, 0, handleResult.Length) |> Async.RunSynchronously
                } |> Async.RunSynchronously
            loop()
        member l.Launch() = 
            l.Start()
            l.Listen()
            l.Launch()
        

