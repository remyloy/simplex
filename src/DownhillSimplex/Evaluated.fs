namespace Optimizer.DownhillSimplex

type Evaluated<'a> = 
    | Evaluated of 'a * cost : float

type Evaluator = Point -> Evaluated<Point>

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Evaluated = 
    let data : Evaluated<'a> -> 'a = fun (Evaluated(x, _)) -> x
    let cost : Evaluated<'a> -> float = fun (Evaluated(_, cost)) -> cost
    
    let min : Evaluated<'a> -> Evaluated<'a> -> Evaluated<'a> = 
        fun a b -> 
            if cost a < cost b then a
            else b
    
    let evaluator : (Point -> float) -> Evaluator = 
        fun evaluator point -> 
            let cost = evaluator point
            Evaluated(point, cost)

    let lift : Evaluator -> (float -> Point -> Point -> Point) -> (float -> Point -> Evaluated<Point> -> Evaluated<Point>) =
        fun evaluator f ->
            fun a b c->
                f a b (data c) |> evaluator
