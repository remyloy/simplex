namespace Optimizer.DownhillSimplex

open Optimizer.DownhillSimplex.Simplex
open Swensen.Unquote
open Xunit

module Simplex =

    [<FactAttribute>]
    let ``Create sorted simplex mean``() = 
        let p1 = 
            { Name = "p1"
              Value = 1.0 }
    
        let p2 = 
            { Name = "p2"
              Value = 2.01 }
    
        let point1 = Point [ p1; p2 ]
        let p1 = { p1 with Value = 1.05 }
        let p2 = { p2 with Value = 2.06 }
        let point2 = Point [ p1; p2 ]
    
        let sorted = 
            Sorted [ Evaluated(point1, 3.01)
                     Evaluated(point2, 3.11) ]
        test <@ Simplex.mean sorted = Point [ { p1 with Value = 0.5 }
                                              { p2 with Value = 1.005 } ] @>

    [<FactAttribute>]
    let ``Create unsorted simplex``() = 
        let p1 = 
            { Name = "p1"
              Value = 1.0 }
    
        let p2 = 
            { Name = "p2"
              Value = 2.01 }
    
        let point1 = Point [ p1; p2 ]
        let p1 = { p1 with Value = 1.05 }
        let p2 = { p2 with Value = 2.06 }
        let point2 = Point [ p1; p2 ]
        test <@ Unsorted.tryCreate [ point1; point2 ] = Some(Unsorted [ point1; point2 ]) @>
