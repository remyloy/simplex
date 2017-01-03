namespace Optimizer.DownhillSimplex

open Option
open Option.Operators

type Grid = 
    | Grid of double []

type Function = 
    | Function of double []
    
    static member simple f (Grid grid) = 
        grid
        |> Array.map f
        |> Function
    
    static member add (Function f1) (Function f2) = Array.map2 (+) f1 f2 |> Function

type ContributionParameter = 
    { a : float
      b : float
      c : float }
    static member Create a b c = 
        { a = a
          b = b
          c = c }

module SampleCurve = 
    let targetParameter = 
        let p1 = ContributionParameter.Create 1.0 2.01 3.01
        let p2 = ContributionParameter.Create 3.99 5.0 6.0
        (p1, p2)
    
    let contribution1 : Grid -> ContributionParameter -> Function = 
        fun grid ps -> Function.simple (fun x -> ps.a + ps.b * x + ps.c * (x * x)) grid
    let contribution2 : Grid -> ContributionParameter -> Function = 
        fun grid ps -> Function.simple (fun x -> ps.a / (ps.b + x) + ps.c * sqrt x) grid
    
    let evaluateFunction : Grid -> (ContributionParameter * ContributionParameter) -> Function = 
        fun grid (p1, p2) -> 
            let f1 = contribution1 grid p1
            let f2 = contribution2 grid p2
            Function.add f1 f2
    
    let residual : Grid -> Function -> (ContributionParameter * ContributionParameter) -> float = 
        fun grid (Function dataFunction) (p1, p2) -> 
            let (Function evaluated) = evaluateFunction grid (p1, p2)
            let cost = Array.map2 (fun t s -> (t - s) * (t - s)) dataFunction evaluated |> Array.sum
            cost
    
    let converters = 
        let contrib1b = "contribution1.b"
        let contrib1c = "contribution1.c"
        let contrib2c = "contribution2.c"
        
        let toPoint : ContributionParameter * ContributionParameter -> Point = 
            fun (p1, p2) -> 
                Point.make [ Parameter.make contrib1b p1.b
                             Parameter.make contrib1c p1.c
                             Parameter.make contrib2c p2.c ]
        
        let toParameters : ContributionParameter * ContributionParameter -> (Point -> ContributionParameter * ContributionParameter) = 
            fun (p1, p2) point -> 
                let update1 b c = 
                    { p1 with b = b
                              c = c }
                
                let update2 c = { p2 with c = c }
                let lookup = Point.lookup point
                let arg name = lookup.TryFind(name)
                let b = arg contrib1b
                let c = arg contrib1c
                let p1 = update1 <!> b <*> c |> Option.withDefault p1
                let c = arg contrib2c
                let p2 = update2 <!> c |> Option.withDefault p2
                p1, p2
        
        (toPoint, toParameters)
