module Problem7

// Find the 10,0001st prime

let MaxFactor n = n |> float |> sqrt |> System.Math.Ceiling |> int


let IsPrime (n: int) : bool = 
    (List.filter (fun x -> n % x = 0) [2..(MaxFactor n)]).Length = 0


let NthPrime n =
    let rec NthPrimeRec ithPrime i curPrimeCandidate =
        if i = n then
            ithPrime
        elif IsPrime curPrimeCandidate then
            NthPrimeRec curPrimeCandidate (i + 1) (curPrimeCandidate + 1)
        else 
            NthPrimeRec ithPrime i (curPrimeCandidate + 1)

    NthPrimeRec 2 1 3


let run = NthPrime 10001

