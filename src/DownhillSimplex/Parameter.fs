namespace Optimizer.DownhillSimplex

type Parameter = 
    { Name : string
      Value : float }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Parameter = 
    let make name value = 
        { Name = name
          Value = value }
    let set value p =
        { p with Value = value }