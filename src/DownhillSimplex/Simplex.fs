namespace Optimizer.DownhillSimplex

module Simplex = 
    type Unsorted<'a> = 
        | Unsorted of 'a
    
    type Sorted<'a> = 
        | Sorted of 'a
    
    type SimplexParameter = 
        { α : float
          β : float
          γ : float
          σ : float }
    
    // TODO: Point muss min. ein Element beinhalten -> Keine leeren Listen erlauben
    // TODO: Alle Listen müssen immer gleich lang sein -> In den Typen muss das garantiert sein -> Prüfen
    // TODO: Alle Listen müssen immer gleich geordnet sein bzgl ihrer Parameternamen -> In den Typen muss das garantiert sein -> Prüfen
    [<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
    module Unsorted = 
        let tryCreate : Point list -> Unsorted<Point list> option = 
            fun parameters -> 
                let lengths = 
                    parameters
                    |> List.map Point.dim
                    |> List.distinct
                if List.length lengths = 1 then Some(Unsorted parameters)
                else None
        
        let generate : Point -> (Parameter -> float) -> float -> Unsorted<Point list> = 
            fun (Point point) characteristicLengthScale simplexConstructionFactor -> 
                let adjustParameter p = 
                    Parameter.set (p.Value + characteristicLengthScale p * simplexConstructionFactor) p
                
                let bases = 
                    point |> List.mapi (fun i _ -> 
                                 point |> List.mapi (fun j p -> 
                                              if i = j then adjustParameter p
                                              else p))
                point :: bases
                |> List.map Point
                |> Unsorted
        
        let map : ('a -> 'b) -> Unsorted<'a> -> Unsorted<'b> = fun f (Unsorted x) -> Unsorted(f x)
    
    [<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
    module Sorted = 
        let sort : Unsorted<Evaluated<Point> list> -> Sorted<Evaluated<Point> list> = 
            fun (Unsorted parameters) -> 
                parameters
                |> List.sortBy Evaluated.cost
                |> Sorted
        
        let best : Sorted<Evaluated<Point> list> -> Evaluated<Point> = fun (Sorted simplex) -> List.head simplex
        let worst : Sorted<Evaluated<Point> list> -> Evaluated<Point> = fun (Sorted simplex) -> List.last simplex
        
        let snd_worst : Sorted<Evaluated<Point> list> -> Evaluated<Point> = 
            fun (Sorted simplex) -> 
                List.rev simplex
                |> List.skip 1
                |> List.head
        
        let updateWorst : Evaluated<Point> -> Sorted<Evaluated<Point> list> -> Unsorted<Evaluated<Point> list> = 
            fun worst (Sorted simplex) -> 
                simplex
                |> List.rev
                |> List.skip 1
                |> List.cons worst
                |> List.rev
                |> Unsorted
        
        let updateRest : (Evaluated<Point> -> Evaluated<Point> -> Evaluated<Point>) -> Sorted<Evaluated<Point> list> -> Unsorted<Evaluated<Point> list> = 
            fun folder (Sorted simplex) -> 
                let best = List.head simplex
                let rest = List.skip 1 simplex
                rest
                |> List.rev
                |> List.map (fun p -> folder best p)
                |> List.cons best
                |> List.rev
                |> Unsorted
    
    let defaultSimplex = 
        { α = 1.0
          β = 0.5
          γ = 2.0
          σ = 0.5 }
    
    let evaluate : Evaluator -> Unsorted<Point list> -> Unsorted<Evaluated<Point> list> = 
        fun evaluator points -> Unsorted.map (List.map evaluator) points
    
    let mean : Sorted<Evaluated<Point> list> -> Point = 
        fun (Sorted parameters) -> 
            parameters
            |> List.rev
            |> List.skip 1
            |> List.map Evaluated.data
            |> Point.mean
    
    let interpolate : float -> Point -> Point -> Point = 
        fun f a b -> Point.sub (Point.scalar (1.0 + f) a) (Point.scalar f b)
    let interpolate2 : float -> Point -> Point -> Point = 
        fun f a b -> Point.add (Point.scalar f a) (Point.scalar (1.0 - f) b)
    let reflect : float -> Point -> Point -> Point = interpolate
    let expand : float -> Point -> Point -> Point = interpolate
    let contract : float -> Point -> Point -> Point = interpolate2
    let compress : float -> Point -> Point -> Point = interpolate2
    
    let step : Evaluator -> SimplexParameter -> Sorted<Evaluated<Point> list> -> Sorted<Evaluated<Point> list> = 
        fun evaluator p simplex -> 
            let mean = mean simplex
            let best = Sorted.best simplex
            let snd_worst = Sorted.snd_worst simplex
            let worst = Sorted.worst simplex
            let reflect = Evaluated.lift evaluator reflect
            let expand = Evaluated.lift evaluator expand
            let contract = Evaluated.lift evaluator contract
            let compress = Evaluated.lift evaluator compress
            
            let nextWorst = 
                let r = reflect p.α mean worst
                if Evaluated.cost r < Evaluated.cost best then 
                    let e = expand p.γ mean worst
                    Some <| Evaluated.min e r
                else if Evaluated.cost r < Evaluated.cost snd_worst then Some r
                else 
                    let h = Evaluated.min r worst
                    let c = contract p.β mean h
                    if Evaluated.cost c < Evaluated.cost worst then Some c
                    else None
            
            let nextSimplex : Evaluated<Point> option -> Sorted<Evaluated<Point> list> -> Unsorted<Evaluated<Point> list> = 
                fun nextWorst -> 
                    match nextWorst with
                    | Some w -> Sorted.updateWorst w
                    | None -> Sorted.updateRest (fun best point -> compress p.σ (Evaluated.data best) point)
            
            let unsorted = nextSimplex nextWorst simplex
            Sorted.sort unsorted
    
    let initial evaluator point characteristicLengthScale simplexConstructionFactor = 
        Unsorted.generate point characteristicLengthScale simplexConstructionFactor
        |> evaluate evaluator
        |> Sorted.sort
    
    let iterations evaluator initial count = 
        [ 1..count ] |> List.scan (fun s _ -> step evaluator defaultSimplex s) initial
