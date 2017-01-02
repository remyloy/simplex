namespace Optimizer.DownhillSimplex

type Point = 
    | Point of Parameter list

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Point = 
    let make : Parameter list -> Point = fun ps -> Point ps
    let run : (Parameter -> 'a) -> Point -> 'a list = fun f (Point ps) -> List.map f ps
    
    let lookup : Point -> Map<string, float> = 
        fun point -> 
            point
            |> run (fun p -> p.Name, p.Value)
            |> Map.ofList
    
    let add : Point -> Point -> Point = 
        fun (Point a) (Point b) -> List.map2 (fun a b -> { a with Value = a.Value + b.Value }) a b |> Point
    let sub : Point -> Point -> Point = 
        fun (Point a) (Point b) -> List.map2 (fun a b -> { a with Value = a.Value - b.Value }) a b |> Point
    let scalar : float -> Point -> Point = 
        fun c (Point a) -> List.map (fun a -> { a with Value = a.Value * c }) a |> Point
    let dim : Point -> int = fun (Point p) -> List.length p
    
    let normalize : Point -> Point = 
        fun point -> 
            let n = 1.0 / float (dim point)
            scalar n point
    
    let mean : Point list -> Point = 
        fun points -> 
            points
            |> List.reduce add
            |> normalize
