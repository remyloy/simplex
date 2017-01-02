namespace Optimizer.DownhillSimplex

open FSharp.Charting
open Option.Operators
open Simplex
open Swensen.Unquote
open System.Diagnostics
open System.Windows.Forms
open Xunit

type Parameters = 
    { a : float
      b : float
      c : float }

module Integration = 
    [<FactAttribute>]
    [<TraitAttribute("Category", "Integration")>]
    let ``Integration test``() = 
        let sw = Stopwatch.StartNew()
        let grid = [| 0.0..0.0001..1.0 |] |> Grid
        let dataFunction = SampleCurve.evaluateFunction grid SampleCurve.targetParameter
        let ps1 = ContributionParameter.Create 1.0 4.01 5.01
        let ps2 = ContributionParameter.Create 3.99 5.0 8.0
        let startParameter = ps1, ps2
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
        
        let chart = 
            states
            |> List.mapi (fun i (Sorted e) -> 
                   (i, 
                    e
                    |> List.head
                    |> Evaluated.cost))
            |> fun data -> Chart.Line(data, Title = runtime.ToString())
        
        let solution = 
            states
            |> List.last
            |> Sorted.best
            |> Evaluated.data
            |> snd SampleCurve.converters startParameter
            |> SampleCurve.evaluateFunction grid
        
        let funcToChart (Grid grid) (Function func) name = 
            Array.map2 (fun x y -> (x, y)) grid func |> fun data -> Chart.Line(data, Name = name)
        
        let combinedChart = 
            Chart.Combine([ funcToChart grid (SampleCurve.evaluateFunction grid startParameter) "initial"
                            funcToChart grid solution "optimized"
                            funcToChart grid dataFunction "target" ]).WithLegend(Enabled = true)
        chart.ShowChart().FormClosed.Add(fun e -> Application.Exit())
        combinedChart.ShowChart() |> ignore
        Application.Run()
        ()
