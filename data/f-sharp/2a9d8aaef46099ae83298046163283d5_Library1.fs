module LibraryUsingTypeProvider

let ctxt = Samples.DataStore.Freebase.FreebaseData.GetDataContext()

let elements = ctxt.``Science and Technology``.Chemistry.``Chemical Elements``

let elementsSorted = 
    elements
    |> Seq.filter (fun e -> e.``Atomic number`` .HasValue)
    |> Seq.sortBy (fun e -> e.``Atomic number``.GetValueOrDefault()) 

let series = ctxt.``Science and Technology``.Chemistry.``Chemical Series``
let series2 = 
    series
    |> Seq.sortBy (fun e -> e.Name)

let discoverers = ctxt.``Science and Technology``.Chemistry.``Chemical Element Discoverers``
let isotopes = ctxt.``Science and Technology``.Chemistry.Isotopes
let compounds = ctxt.``Science and Technology``.Chemistry.``Chemical Compounds``
let bonds = ctxt.``Science and Technology``.Chemistry.``Chemical Bonds``
