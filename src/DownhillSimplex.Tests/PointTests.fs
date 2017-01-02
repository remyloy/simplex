namespace Optimizer.DownhillSimplex

open Swensen.Unquote
open Xunit

module Point =

    [<FactAttribute>]
    let ``Point add``() = 
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
        test <@ Point.add point1 point2 = Point [ { Name = "p1"
                                                    Value = 2.05 }
                                                  { Name = "p2"
                                                    Value = 4.07 } ] @>

    [<FactAttribute>]
    let ``Point scalar``() = 
        let p1 = 
            { Name = "p1"
              Value = 1.0 }
    
        let p2 = 
            { Name = "p2"
              Value = 2.01 }
    
        let point1 = Point [ p1; p2 ]
        test <@ Point.scalar 2.0 point1 = Point [ { Name = "p1"
                                                    Value = 2.0 }
                                                  { Name = "p2"
                                                    Value = 4.02 } ] @>

    [<FactAttribute>]
    let ``Point dim``() = 
        let p1 = 
            { Name = "p1"
              Value = 1.0 }
    
        let p2 = 
            { Name = "p2"
              Value = 2.01 }
    
        let point1 = Point [ p1; p2 ]
        test <@ Point.dim point1 = 2 @>

    [<FactAttribute>]
    let ``Point normalize``() = 
        let p1 = 
            { Name = "p1"
              Value = 1.0 }
    
        let p2 = 
            { Name = "p2"
              Value = 2.01 }
    
        let point1 = Point [ p1; p2 ]
        test <@ Point.normalize point1 = Point [ { p1 with Value = 0.5 }
                                                 { p2 with Value = 1.005 } ] @>
