// a type representing the amount of a specific drink
type DrinkAmount =
    | Coffee of int
    | Tea of int
    | Water of int
    with
        // get a string representation of the value
        override x.ToString() =
            match x with
            | Coffee x -> Printf.sprintf "Coffee: %i" x
            | Tea x -> Printf.sprintf "Tea: %i" x
            | Water x -> Printf.sprintf "Water: %i" x

// create a new instance of DrinkAmount
let t = Tea 2

// print out the string
printfn "%s" (t.ToString())