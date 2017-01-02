namespace Optimizer.DownhillSimplex

module Option = 
    let retn = Some
    
    let apply optF optX = 
        match optF, optX with
        | Some f, Some x -> Some(f x)
        | Some _, None -> None
        | None, Some _ -> None
        | None, None -> None
    
    let withDefault def opt = 
        match opt with
        | Some x -> x
        | None -> def
    
    module Operators = 
        let (<!>) a b = Option.map a b
        let (<*>) a b = apply a b
