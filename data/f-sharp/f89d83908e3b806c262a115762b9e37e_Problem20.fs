module Problem20

open ReallyBigInt

// Find sum of the digits in 100!

let BigFactorial (n: int) : BigInt =
    let mutable f = new BigInt(1)
    for i in [2..n] do
        f <- f * BigInt(i)
    f

// Add implementation of * operator to ReallyBigInt class
let run = BigFactorial(100).SumDigits

