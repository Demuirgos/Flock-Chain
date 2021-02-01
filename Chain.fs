module Chain 
    open System
    open Contract
    open Block
    
    let Difficulty = Easy

    type Chain = 
        {
            Pending     : Contract list
            Difficulty  : Difficulty
            Blocks      : Block<Contract> list
        }
        member this.Reward =  1 
        static member Genesis difficulty = 
            {
                Pending    = []
                Difficulty = difficulty
                Blocks     = [ genesis ]  
            }

    let transact chain contracts = match chain.Blocks.IsEmpty with 
                                   | false ->  Ok {
                                                 chain with Pending = contracts @ chain.Pending
                                              }
                                   | _    -> Error("Chain has no genesis Block")

    let pay miner ammount = {
        From    = System("Dummy Identifier")
        To      = miner
        Ammount = ammount
    }


    let rec Process miner chainStream  = 
        match chainStream with 
        | Ok(chain) -> Ok {
                            chain with 
                                        Blocks  = 
                                            let block = ((List.length chain.Blocks), {
                                                            Date     = DateTime.Now ;
                                                            Previous = chain.Blocks.Head.Hash;
                                                            Datum     = chain.Pending |> List.map (fun x -> Some(x)) ;
                                                        },chain.Difficulty)
                                                        |||> Block.create 
                                            block::chain.Blocks
                                        Pending = [pay miner chain.Reward] ;
                        } 
        | Error(msg) as e -> e


    let add (chain:Chain) datum = 
        match datum with 
        | None -> Error("Empty Block is invalid")
        | Some(v) -> match chain.Blocks with 
                     | c::_ -> 
                        let block = (c.Index + 1, v, chain.Difficulty) |||> Block.create
                        Ok({ chain with
                                        Blocks = block::chain.Blocks
                        })
                     | _ -> Error("Chain has no genesis")

    let validate (chain:Chain)=
        let rec check blocks= 
            match blocks with 
            | []      -> false
            | b1::(b2::_ as bs) -> 
                if (b1.Hash <> hash b1.Data b1.Nonce) || (b1.Data.Previous <> b2.Hash) then
                    false
                else
                    check bs
            | _ -> true
        check chain.Blocks