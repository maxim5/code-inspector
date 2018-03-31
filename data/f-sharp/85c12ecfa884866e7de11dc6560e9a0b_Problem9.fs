module Problem9

open Xunit

// There is one Pythagorean triple for which a + b + c = 1000
// Find the product abc

let rec AllPairs l =
    match l with
    | [] -> []
    | x::xs -> (List.map (fun e -> (x, e)) l) @ (AllPairs xs)

let PythagTriple a b =
    let possibleC = a * a + b * b |> float |> sqrt |> System.Math.Ceiling |> int
    if possibleC * possibleC = a * a + b * b then
        Some(possibleC)
    else
        None

let PythagTripleProduct n =
    [2 .. n]
    |> AllPairs
    |> Seq.map (fun (a, b) -> (a, b, (PythagTriple a b)))
    |> Seq.filter (fun (a, b, pt) -> pt.IsSome)
    |> Seq.map (fun (a, b, pt) -> (a, b, pt.Value))
    |> Seq.filter (fun (a, b, c) -> a + b + c = n)
    |> Seq.map (fun (a, b, c) -> a * b * c)
    |> (fun x -> if Seq.length(x) > 0 then Some(Seq.head(x)) else None)

let run = PythagTripleProduct 1000

// Tests
[<Fact>]
let test_AllPairs () =
    Assert.Equal((AllPairs [1; 2]), 
                 [(1, 1); (1, 2); (2, 2)])

[<Fact>]
let test_AllPairs2 () =
    Assert.Equal((AllPairs [1; 2; 3]), 
                 [(1, 1); (1, 2); (1, 3); (2, 2); (2, 3); (3, 3)])

[<Fact>]
let test_PythagTriple () = Assert.Equal((PythagTriple 3 4), Some(5))

[<Fact>]
let test_PythagTriple2 () = Assert.Equal((PythagTriple 3 5), None)

[<Fact>]
let test_PythagTripleProduct () =
    Assert.Equal((PythagTripleProduct (3 + 4 + 5)),
                 Some(3 * 4 * 5))

[<Fact>]
let test_PythagTripleProduct2 () =
    Assert.Equal((PythagTripleProduct (5 + 12 + 13)),
                 Some(5 * 12 * 13))