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
        static member Genesis difficulty = 
            {
                Pending    = []
                Difficulty = difficulty
                Blocks     = [ genesis ]  
            }

    let add (chain:Chain) datum = 
        match datum with 
        | None -> Error("Empty Block is invalid")
        | Some(v) -> match chain.Blocks with 
                     | c::_ -> 
                        let block = (c.Index + 1, v, chain.Difficulty) |||> Block.Create
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