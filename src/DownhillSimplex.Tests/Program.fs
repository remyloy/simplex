open FSharp.Charting
open Optimizer.DownhillSimplex
open Optimizer.DownhillSimplex.Simplex
open System.Diagnostics
open System.Windows.Forms

type Parameters = 
    { a : float
      b : float
      c : float }

let ps1 = ContributionParameter.Create 1.0 4.01 5.01
let ps2 = ContributionParameter.Create 3.99 5.0 8.0
let startParameter = ps1, ps2

let prepare() = 
    let sw = Stopwatch.StartNew()
    let grid = [| 0.0..0.0001..1.0 |] |> Grid
    let dataFunction = SampleCurve.evaluateFunction grid SampleCurve.targetParameter
    let simplexConstructionFactor = 0.5
    let characteristicLengthScale (v : Parameter) = abs v.Value * 0.5
    
    let evaluate : Point -> float = 
        fun point -> 
            let ps = snd SampleCurve.converters startParameter point
            SampleCurve.residual grid dataFunction ps
    
    let evaluator = Evaluated.evaluator evaluate
    let initial = 
        Simplex.initial evaluator (fst SampleCurve.converters startParameter) characteristicLengthScale 
            simplexConstructionFactor
    let states = Simplex.iterations evaluator initial 50
    let runtime = sw.Elapsed
    (states, runtime)

[<EntryPointAttribute>]
let main _ = 
    let runs = [ 1..100 ] |> List.map (fun _ -> prepare())
    
    let states = 
        runs
        |> List.last
        |> fst
    
    let min = 
        runs
        |> List.map snd
        |> List.min
    
    let max = 
        runs
        |> List.map snd
        |> List.max
    
    let runtime = 
        runs
        |> List.map snd
        |> List.reduce (+)
        |> fun ts -> ts.TotalSeconds / float runs.Length |> System.TimeSpan.FromSeconds
    
    let chart = 
        states
        |> List.mapi (fun i (Sorted e) -> 
               (i, 
                e
                |> List.head
                |> Evaluated.cost))
        |> fun data -> Chart.Line(data, Title = (sprintf "avg %A min %A max %A" runtime min max))
    
    chart.ShowChart().FormClosed.Add(fun _ -> Application.Exit())
    Application.Run()
    0
