// a sequence of even numbers
let evens n =
    seq { for x in 1 .. n do 
            if x % 2 = 0 then yield x }
    
// print the sequence
printfn "%A" (evens 10)