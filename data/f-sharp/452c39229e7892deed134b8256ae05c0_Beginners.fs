// Copyright (c) Microsoft Corporation 2005-2009

module Beginners
open Sample.Support
open System
open System.Collections.Generic

//--------------------------------------------------------

[<Category("Arithmetic");
  Title("Integer Arithmetic");
  Description("This sample shows some basic integer arithmetic")>]
let SampleArithmetic1() =
    let x = 10 + 12 - 3 
    let y = x * 2 + 1 
    let r1,r2 = x/3, x%3
    printfn "x = %d, y = %d, r1 = %d, r2 = %d" x y r1 r2
  

[<Category("Arithmetic");
  Title("Floating Point Arithmetic");
  Description("This sample shows some basic floating point arithmetic")>]
let SampleArithmetic2() =
    let x = 10.0 + 12.0 - 3.0 
    let y = x * 2.0 + 1.0 
    let r1 = x/3.0
    printfn "x = %g, y = %g, r1 = %g" x y r1

  
[<Category("Arithmetic");
  Title("Converting between numeric types");
  Description("This sample shows how to convert between various numeric types")>]
let SampleArithmetic3() =
    // Manipulating double-precision (64-bit) floating point numbers
    let pi1 = float 3  + 0.1415   // 'float' is an overloaded conversion operator
    let pi2 = double 3 + 0.1415   // identical - 'double' is a synonym for 'float'
    printfn "pi1 = %f, pi2 = %f" pi1 pi2
  
    let i1 = int 3.1415 
    let i2 = int64   3.1415 
    printfn "i1 = %d, i2 = %d" i1 i2
  
    // Manipulating single-precision (32-bit) floating point numbers
    let f32a = 2.1415f + 1.0f            // float32 (System.Single)
    let f32b = 2.1415f + float32 1       // float32 - identical 
    printfn "f32a = %f, f32b = %G" f32a f32b
  
    // Manipulating bytes
    let byteA = byte (3+4)          // byte 
    let byteB = 255uy               // byte 
    let byteC = 0xFFuy              // byte 
    let byteD = byte 0xFF           // byte 
    printfn "byteA = %d, byteB = %d" byteA byteB
  
//--------------------------------------------------------

[<Category("Functions");
  Title("Declaring and calling inner functions");
  Description("Declaring and calling functions within the body of another function")>]
let FunctionSample1() =
    let twice x = x + x 
    printfn "twice 2 = %d" (twice 2)
    printfn "twice 4 = %d" (twice 4)
    printfn "twice (twice 2) = %d" (twice (twice 2))

[<Category("Functions");
  Title("Declaring and calling inner functions");
  Description("Declaring and calling functions within the body of another function")>]
let FunctionSample2() =
    let even n = (n%2 = 0) 
    let tick x = printfn "tick %d" x 
    let tock x = printfn "tock %d" x 
    let choose f g h x = if f x then g x else h x 
    let ticktock = choose even tick tock  // ticktock is a function built out of other functions using 'choose'
    for i = 0 to 10 do
        ticktock i
    

[<Category("Functions");
  Title("Declaring and calling anonymous function lamda expressions");
  Description("Introduction to using lambda expressions")>]
let FunctionSample3() =
    let tick x = printfn "tick %d" x 
    let tock x = printfn "tock %d" x 
    let choose f g h x = if f x then g x else h x 
    for i = 0 to 10 do
        // This is like the previous sample, but uses an anonymous lambda expression for 
        // the function that decides whether to tick or tock.
        choose (fun n -> n%2 = 0) tick tock i 
    
//--------------------------------------------------------

[<Category("Exceptions");
  Title("Raising a 'Failure' exception");
  Description("How to raise a simple exception")>]
let ExceptionSample1() : unit =
    failwith "Here's how to raise a simple 'Failure' exception"
  

[<Category("Exceptions");
  Title("Raising and catching a 'Failure' exception");
  Description("How to raise and catch a 'Failure' exception")>]
let ExceptionSample2() =
    try 
        printfn "About to raise a simple 'Failure' exception..."
        failwith "Whoa!"
    with 
        Failure msg -> 
            printfn "Caught a simple 'Failure' exception, msg = '%s'" msg
  
[<Category("Exceptions");
  Title("Using KeyNotFoundException and exception patterns");
  Description("Raising a 'KeyNotFoundException' exception, and using exception patterns")>]
let ExceptionSample3() =
    try 
        printfn "About to raise an exception..."
        match DateTime.Now.DayOfWeek with 
        | DayOfWeek.Monday -> raise(KeyNotFoundException()) 
        | _                -> failwith "it's not Monday"
    with 
        | :? KeyNotFoundException -> 
            printfn "Caught a 'KeyNotFoundException' exception, it must be Monday"
        | Failure msg-> 
            printfn "Caught a 'Failure' exception: %s" msg

[<Category("Exceptions");
  Title("Using ArgumentException and exception patterns");
  Description("Raising an 'ArgumentException' exception, and filtering for .NET exceptions")>]
let ExceptionSample4() =
    try 
        printfn "About to raise an ArgumentException exception..."
        if DateTime.Now.DayOfWeek = DayOfWeek.Tuesday then 
            raise (new System.ArgumentException("Not today, it's Tuesday"))
        else
            raise (new System.ApplicationException("Hey, it's not Tuesday..."))
    with 
        | :? System.ArgumentException as e -> 
            printfn "Caught an ArgumentException, e.Message = %s" e.Message
        | :? System.ApplicationException as e -> 
            printfn "Caught an ApplicationException, e.Message = %s" e.Message
        | _ -> 
            printfn "Some other exception was caught"
  
//--------------------------------------------------------
  
[<Category("For and While Loops");
  Title("For Loops");
  Description("Some simple 'for' loops")>]
let SampleForLoop1() =
    for i in ["quick"; "brown"; "fox" ]  do 
        printfn "i = %s" i
    
[<Category("For and While Loops");
  Title("For Loops");
  Description("Some simple 'for' loops")>]
let SampleForLoop2() =
    for i in 1 .. 10 do 
        printfn "In a for-loop, i = %d" i
     
[<Category("For and While Loops");
  Title("For Loops");
  Description("Some simple nested 'for' loops")>]
let SampleForLoop3() =
    for i in 0 .. 9 do 
        for j = 0 to i-1 do 
            printfn " "
        for j = i to 9 do 
            printfn "%d" j
        printfn ""
    
[<Category("For and While Loops");
  Title("While Loops");
  Description("A simple 'while' loop that counts, skipping by 2")>]
let SampleWhileLoop1() =
    let count = ref 0
    while (!count < 10) do 
        printfn "Counting, skipping by 2, count = %d..." !count;
        count := !count + 2
    printfn "Done counting!"
  
[<Category("For and While Loops");
  Title("While Loops");
  Description("A simple 'while' loops that busy-waits until the given time-span has passed")>]
let SampleWhileLoop2() =
    let start = DateTime.Now 
    let duration = System.TimeSpan.FromMilliseconds(8.0)
    let diff (a:DateTime) (b:DateTime) = System.DateTime.op_Subtraction(System.DateTime.Now,b) 
    printfn "Waiting..."

    // Here's the loop
    while diff DateTime.Now start < duration do
        printfn "."

    // OK, we're done...
    let span = diff DateTime.Now start 
    printfn "\nAttempted to busy-wait 8ms, actually waited %dms" span.Milliseconds

//--------------------------------------------------------

[<Category("Recursion");
  Title("Declaring a simple recursive function");
  Description("Declaring a simple recursive function")>]
let SampleRec1() =
    let rec fib n = if n < 2 then 1 else fib (n-1) + fib (n-2) 
    for i = 1 to 10 do
        printfn "fib %d = %d" i (fib i)

//--------------------------------------------------------

[<Category("Arrays, Hash Tables and Dictionaries");
  Title("Basic Array Construction");
  Description("This sample demonstrates basic array construction.")>]
let BasicConstruction() =

    let emptyA = [||]
    
    let smallNumsA = [|1; 2; 3|]
    
    let emptyA2: int array = Array.zeroCreate 5
    
    printfn "emptyA = %A" emptyA
    printfn "\nsmallNumsA = %A" smallNumsA
    printfn "\nemptyA2 = %A" emptyA2

[<Category("Arrays, Hash Tables and Dictionaries");
  Title("Basic Array Construction 2");
  Description("This sample demonstrates more basic array construction.")>]
let BasicConstruction2() =
    
    // Create an array using a range expression
    // general form: let <name> = [|startExpr..[stepExpr]..endExpr|]
    // where [stepExpr] is optional and has default value of 1
    let arrRngExp = [|0..5|]

    let arrRngExp2 = [|1..2..9|]

    // Create an array using a function based on index
    let squaresByInit = Array.init 5 (fun i -> (i, i*i))

    // Create an array using array comprehension
    let squaresByComp = [| for i in 0..5 do yield (i, i*i) |]
           
    printfn "arrRngExp = %A" arrRngExp
    printfn "\narrRngExp2 = %A" arrRngExp2
    printfn "\nsquaresByInit = %A" squaresByInit
    printfn "\nsquaresByComp = %A" squaresByComp
    
    // Comprehensions can contain arbitrary code
    let arbitraryComp = 
            [| for i in 1..10 do
                    if i <> 7 then
                        printfn "%d" i
                        yield i
               printfn "did thru 10, skipped 7"
               yield 100 |]
    

    printfn "\narbitraryComp = %A" arbitraryComp

[<Category("Arrays, Hash Tables and Dictionaries");
  Title("Using arrays");
  Description("")>]
let SampleArray1() =
    let size = 1000 
    let arr = Array.create size 0 
    for i = 1 to size - 1 do 
        arr.[i] <- i + arr.[i-1]
    for i = 1 to size - 1 do 
        printfn "arr.[%4d] = %d" i arr.[i]
    
[<Category("Arrays, Hash Tables and Dictionaries");
  Title("Using arrays");
  Description("Create a histogram of the occurrences of particular letters")>]
let SampleArray2() =
    let numLetters = 26 
    let results = Array.create numLetters 0 
    let data = "The quick brown fox jumps over the lazy dog" 
    for i = 0 to data.Length - 1 do 
        let c = data.Chars(i) 
        let c = Char.ToUpper(c)  
        if c >= 'A' && c <= 'Z' then 
            let i = int c - int 'A' 
            results.[i] <- results.[i] + 1
    for i = 0 to numLetters - 1 do 
        printfn "Number of '%c' characters = %d" (char (i + int 'A')) results.[i]
 
[<Category("Arrays, Hash Tables and Dictionaries");
  Title("Using System.Collections.Generic.Dictionary");
  Description("Create a histogram of the occurrences of particular unicode characters using a dictionary.")>]
let SampleHashtbl3() =
    let tab = new System.Collections.Generic.Dictionary<char,int>(30) 
    let data = "The quick brown fox jumps over the lazy dog" 
    for i = 0 to data.Length - 1 do 
        let c = data.Chars(i) 
        if tab.ContainsKey(c) then 
            let v = tab.[c] 
            let _ = tab.Remove(c) 
            tab.Add(c,v+1)
        else 
            tab.Add(c,1)

    for KeyValue(k,v) in tab do
        printfn "Number of '%c' characters = %d" k v

//-------------------------------------------------------- 

[<Category("Lists, Tuples and Options");
  Title("Using Tuples");
  Description("This sample shows simple uses of tuples")>]
let TupleSample1() =
    let data = 1,2,3
    printfn "data = \n%A" data
    let f (a,b,c) = (a+b,b+c,c+a) 
    let res = f(f(f(data))) 
    printfn "res = \n%A" res
    let r1,r2,r3 = res 
    printfn "r1 = %d, r2 = %d, r3 = %d" r1 r2 r3
    let r4,r5,r6 = f(res) 
    printfn "r4 = %d, r5 = %d, r6 = %d" r4 r5 r6
  
[<Category("Lists, Tuples and Options");
  Title("Basic List Construction");
  Description("This sample demonstrates basic list construction")>]
let ListConstruction() =

    // Create an empty list
    let emptyL = []

    // Create a simple floating-point number list
    let smallFloatsL = [1.0; 2.0; 3.0; 4.0]

    // Create a list using a range expression - same rules as with arrays, step is optional
    let listRngExp = [0..2..10]

    // Create a list using a function based on index
    let listByInit = List.init 5 (fun i -> i*i*i)

    // Create a list using list comprehension
    let listLC = [ for i in 1..5 do yield (i * 2) ]
       
    // Another example of list comprehension
    let listLC2 = [ for p in [(0,0); (1,1); (2,4); (3,9); (4,16); (5,25); (6, 36)] do
                        match p with
                        | (a,_) when a%2=0 -> yield p
                        | (_,_) -> () ]
                        
    printfn "emptyL = %A" emptyL
    printfn "\nsmallFloatsL = %A" smallFloatsL
    printfn "\nlistRngExp = %A" listRngExp
    printfn "\nlistByInit = %A" listByInit
    printfn "\nlistLC = %A" listLC
    printfn "\nlistLC2 = %A" listLC2

[<Category("Lists, Tuples and Options");
  Title("Basic List Construction 2");
  Description("This sample demonstrates a simple generative list comprehension to specify lists")>]
let SequenceExpressionSample1() =
    
    let data1 = [ for x in 0..20 -> x, x * x ]

    // This uses a nested loop
    let data2 = [ for x in 0..5 do
                    for y in 0..5 do
                      yield x, y, x * y ]

    // This uses a filter
    let data3 = [ for x in 0..5 do
                    for y in 0..5 do
                      if x > y then 
                        yield (x, y, x * y) ]

    // This uses an internal let-binding
    let data4 = [ for x in 0..5 do
                    for y in 0..5 do
                      if x > y then 
                        let z = x * y 
                        yield (x, y, z) ]
    
    printfn "data1 = \n%A\n\n" data1
    printfn "data2 = \n%A\n\n" data2
    printfn "data3 = \n%A\n\n" data3
    printfn "data4 = \n%A\n\n" data4

[<Category("Lists, Tuples and Options");
  Title("Lists and Pattern Matching");
  Description("This sample shows simple uses of lists and functions that pattern match on them.")>]
let ListSample1() =
    let data = [1;2;3;4]
    printfn "data = \n%A" data
    printfn "List.head data = %d" (List.head data)
    printfn "List.tail data = \n%A" (List.tail data)
    printfn "List.length data = %d" (List.length data)
    let consume data = 
        match data with 
        | 1::rest    -> printfn "matched a 1";       rest
        | 2::3::rest -> printfn "matched a 2 and 3"; rest 
        | [4]        -> printfn "matched a 4";       []
        | _          -> printfn "unexpected!";         [] 
    let data = consume data 
    let data = consume data 
    let data = consume data 
    printfn "At end of list? %b" (data = [])
  
[<Category("Lists, Tuples and Options");
  Title("Lists and 'map'");
  Description("This sample shows simple uses of 'map'")>]
let ListSample2() =
    let data = [1;2;3;4]
    let r1 = data |> List.map (fun x -> x + 1)
    printfn "Adding '1' using map = %A" r1
    let r2 = data |> List.map string
    printfn "Converting to strings using map = %A" r2
    let r3 = data |> List.map (fun x -> (x,x))
    printfn "Tupling up using map = %A" r3
 
[<Category("Lists, Tuples and Options");
  Title("Lists and Iteration");
  Description("This sample shows simple uses of 'iter'")>]
let ListSample3() =
    let data = ["Cats";"Dogs";"Mice";"Elephants"]
    data |> List.iter (fun x -> printfn "item: %s" x)
 
[<Category("Lists, Tuples and Options");
  Title("Lists and Indexed Iteration");
  Description("This sample shows simple uses of 'iteri'")>]
let ListSample4() =
    let data = ["Cats";"Dogs";"Mice";"Elephants"]
    data |> List.iteri (fun i x -> printfn "item %d: %s" i x)
 

[<Category("Lists, Tuples and Options");
  Title("Lists and Folding");
  Description("This sample shows simple uses of 'fold' to accumulate a result over a list")>]
let ListSample5() =
    let data = [("Cats",4);
                ("Dogs",5);
                ("Mice",3);
                ("Elephants",2)]
    let count = List.fold (fun acc (nm,x) -> acc+x) 0 data
    printfn "Total number of animals: %d" count
 
[<Category("Lists, Tuples and Options");
  Title("Lists and Filter");
  Description("This sample shows how to filter a list")>]
let ListSample6() =
    let data = [("Cats",4);
                ("Dogs",5);
                ("Mice",3);
                ("Elephants",2)]
    let res = data |> List.filter (fun (nm,x) -> nm.Length <= 4)
    printfn "Animals with short names: %A" res
 

[<Category("Lists, Tuples and Options");
  Title("Lists and Choose");
  Description("Project from a list")>]
let ListSample7() =
    let data = [("Cats",4);
                ("Dogs",5);
                ("Mice",3);
                ("Elephants",2)]
    let res = data |> List.choose (fun (nm,x) -> if nm.Length <= 4 then Some(x) else None)
    printfn "Counts of animals with short names: %A" res
 
  
[<Category("Lists, Tuples and Options");
  Title("Simple options");
  Description("Create some optional values, print them and match on them.")>]
let OptionsSample2() =

    let data = Some(1,3)
    printfn "data = %A" data;
    printfn "data.IsSome = %b" data.IsSome
    printfn "data.IsNone = %b" data.IsNone
    printfn "data.Value = %A" data.Value

    let data2 = None
    printfn "data2.IsSome = %b" data2.IsSome
    printfn "data2.IsNone = %b" data2.IsNone

[<Category("Lists, Tuples and Options");
  Title("Using options for data");
  Description("Here we return an option from a function indicating the opening hours of a shop, if any.")>]
let OptionsSample3() =

    let openingHours day = 
        match day with 
        | DayOfWeek.Monday 
        | DayOfWeek.Tuesday 
        | DayOfWeek.Thursday 
        | DayOfWeek.Friday    -> Some(9,17)
        | DayOfWeek.Wednesday -> Some(9,19) // extended hours on Wednesday
        | _ -> None 

    let today = DateTime.Now.DayOfWeek 

    match openingHours today with 
    | None -> printfn "The shop's not open today"
    | Some(s,f) -> printfn "The shop's open today from %02d:00-%d:00" s f

//--------------------------------------------------------

[<Category("Sequences");
  Title("Basic Construction");
  Description("This sample demonstrates basic sequence construction.")>]
let SequenceExample1() =
    // Create an empty sequence
    let emptySeq = Seq.empty

    // Create a simple sequence
    let simpleSeq = seq{1..50}      
        // sequence terms are not evaluated until needed

    // Create a sequence using a sequence expression
    let squaresSeq = seq { for i in 1..5 do yield i*i }              

    // Alternate form of the previous sequence expression
    let squaresSeq2 = seq { for i in 1..5 -> i*i }      
        // '->' is equivalent to 'do yield'

    // Another sequence expression example
    let getDirs dir = seq { for folder in System.IO.Directory.GetDirectories(dir) -> folder }
    
    printfn "emptySeq = %A" emptySeq
    printfn "\nsimpleSeq = %A" simpleSeq
    printfn "\nsquaresSeq = %A" squaresSeq
    printfn "\nsquaresSeq2 = %A" squaresSeq2
    printfn "\nget directories in C: %A" (getDirs @"C:\")       
    
[<Category("Sequences");
  Title("Basic Iteration");
  Description("This sample demonstrates basic sequence iteration.")>]
let SequenceExample2() =
    let squaresSeq = seq {for i in 1..5 -> i*i}
    // iterating a sequence in this manner can cause exceptions/errors with large sequences!
    printf "squaresSeq = "
    for i in squaresSeq do printf "%d " i

//--------------------------------------------------------

[<Category("Functions - Arrays, Lists, Seqs");
  Title("Using .iter");
  Description(".iter applies a given function to each element in the collection, returning a value of unit ().")>]
let AggregateExample1() =
    let aoList = [ for i in 1..5 -> (i, i*i) ]
    let aoArray = [| for i in 1..5 -> (i, i*i) |]
    let aoSeq = seq { for i in 1..5 -> (i, i*i) }
    
    printf "aoList: "
    List.iter (fun (a,b) -> printf "(%d, %d) " a b) aoList
    printf "\naoArray: "
    Array.iter (fun (a,b) -> printf "(%d, %d) " a b) aoArray
    printf "\naoSeq: "
    Seq.iter (fun (a,b) -> printf "(%d, %d) " a b) aoSeq
    
[<Category("Functions - Arrays, Lists, Seqs");
  Title("Using .map");
  Description(".map applies a given function to each element in the collection, returning a new collection.")>]
let AggregateExample2() =
    // can be applied to arrays and sequences as well
    let aoList2 = List.map (fun (a,b,c) -> 
            let date = new System.DateTime(c, a, b)
            date.ToString("F")) [(1,1,2001); (2,2,2004); (6,17,2009)]
            
    for i in aoList2 do printfn "%A" i

[<Category("Functions - Arrays, Lists, Seqs");
  Title("Using .reduce");
  Description(".reduce applies a given function to the collection, threading an accumulator argument through and returning a single value.")>]
let AggregateExample3() =
    let aoList2 = [ for i in 1..5 -> i*i ]
    let add acc item = acc + item       // accumulator function 
    let sum = List.reduce add aoList2

    let names = [| "A"; "man"; "landed"; "on"; "the"; "moon" |]
    let insertComma (acc: string) item = acc + " " + item
    let sentence = Array.reduce insertComma names
    
    printfn "sum = %d" sum
    printfn "\nsentence = %s" sentence

[<Category("Functions - Arrays, Lists, Seqs");
  Title("Using .filter");
  Description(".filter applies a given function to the collection, returning only those elements for which the given predicate returns true.")>]
let AggregateExample4() =
    let names = [|"Bob"; "Jim"; "Stephen"; "James"; "Fred"; "Will"; "Brian"; "David"; "Mark"; "Jonathan"|]
    let longNames = Array.filter (fun (x: string) -> x.Length > 4) names
    
    printfn "names = %A\n" names
    printfn "longNames = %A" longNames

[<Category("Functions - Arrays, Lists, Seqs");
  Title("Using .choose");
  Description(".choose applies a given function to the collection, returning an array of results x where the function evaluates to Some(x).")>]
let AggregateExample5() =
    let numbers = seq {1..20}
    let evens = Seq.choose(fun x -> 
                                match x with
                                | x when x%2=0 -> Some(x)
                                | _ -> None ) numbers
    printfn "numbers = %A\n" numbers
    printfn "evens = %A" evens

//--------------------------------------------------------  

[<Category("Equality, Comparison and Hashing");
  Title("Using Structural Equality");
  Description("F# supports structural comparison and equality of values with the same type.  This sample shows the results of some simple structural comparison operations.")>]
let ComparisonSample1() =
    let show a b = 
        printfn "%A < %A: %b" a b (a < b)
        printfn "%A = %A: %b" a b (a = b)
        printfn "%A > %A: %b" a b (a > b)
    
    show 1 2;
    show 2 2;
    show "1" "2"
    show "abb" "abc" 
    show "aBc" "ABB" // case-sensitive
    show None (Some 1);
    show None None;
    show (Some 0) (Some 1);
    show (Some 1) (Some 1);
    show [1;2;3] [1;2;2];
    show [] [1;2;2]
  

[<Category("Equality, Comparison and Hashing");
  Title("Using Structural Hashing");
  Description("F# supports structural hashing on values. Typically only F# record/union structured terms are traversed, though this can be customized on a per-type basis. This sample shows the results of some simple structural hashing operations.")>]
let HashingSample2() =
    let show a = printfn "hash(%A) : %d" a (hash a) 
    show 1;
    show 2;
    show "1"
    show "2"
    show "abb" 
    show "aBc" // case-sensitive
    show None;
    show (Some 1);
    show (Some 0);
    show [1;2;3];
    show [1;2;3;4;5;6;7;8];
    show [1;2;3;4;5;6;7;8;9;10;11];
    show [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15]
    
//--------------------------------------------------------

[<Support("UnionSample1")>]
let dummy() = ()
type wheel = Wheel of float  // radius of wheel, inches
type cycle = 
    | Unicycle of wheel
    | Bicycle of wheel * wheel 

let veryBigWheel = Wheel(26.0)
let bigWheel     = Wheel(13.0)
let smallWheel   = Wheel(6.0)

let pennyFarthing = Bicycle(veryBigWheel, smallWheel)
let racer         = Bicycle(bigWheel, bigWheel)
let kidsBike      = Bicycle(smallWheel, smallWheel)

[<Category("Defining Types");
  Title("Basic Discriminated Unions");
  Description("Discriminated unions give a way of building types from the disjoint union of two or more existing types. This sample shows how to build one such type and how to decompose its values.")>]
let UnionSample1() =
    let show bike = 
        match bike with 
        | Unicycle (Wheel r) -> printfn "Unicycle, one wheel, radius = %f" r
        | Bicycle (Wheel r1,Wheel r2) -> printfn "Bicycle, two wheels, front = %f, back = %f" r1 r2 
    show pennyFarthing;
    show racer;
    show kidsBike

[<Support("UnionSample2")>]
let dummy2() = ()
type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Tip

[<Category("Defining Types");
  Title("Discriminated Unions as Trees");
  Description("Discriminated unions are excellent for representing tree structures.")>]
let UnionSample2() =
    let tree = Node(4, Node(2, Node(1, Tip, Tip), Node(6, Tip, Tip)),
                       Node(6, Node(5, Tip, Tip), Node(7, Tip, Tip)))
    //      4
    //  2       6
    // 1 3     5 7
    // InOrder : Tree<'a> -> list<'a>    
    let rec InOrder t = 
        match t with 
        | Node(x,left,right) -> (InOrder left) @ [x] @ (InOrder right) 
        | Tip -> [] 

    // Height : Tree<'a> -> int 
    let rec Height t = 
        match t with 
        | Node(x,left,right) -> 1 + (max (Height left) (Height right)) 
        | Tip -> 0
        
    printfn "The tree in order = %A" (InOrder tree)
    printfn "\nThe height of the tree = %d" (Height tree)


[<Support("RecordSample1")>]
let dummy3() = ()
type Point = { x: float; y: float}
type Triangle = { p1: Point; p2: Point; p3: Point }
type Vector = { dx: float; dy: float}

let origin = { x = 0.0; y = 0.0 }
let onex = { x = 1.0; y = 0.0 }
let oney = { x = 0.0; y = 1.0 }
let diff p1 p2 = { dx = p2.x - p1.x; dy = p2.y - p1.y }
let sides tri = 
    diff tri.p2 tri.p1, 
    diff tri.p3 tri.p2, 
    diff tri.p1 tri.p3

[<Category("Defining Types");
  Title("Basic Records");
  Description("Records are concrete type definitions that hold data in an unordered, named structure.")>]
let RecordSample1() =
    let triangle1 = { p1=origin;p2=onex;p3=oney } 
    printfn "triangle1 = \n%A" triangle1;
    printfn "sides(triangle1) = \n%A" (sides triangle1)

[<Support("RecordSample2")>]
let dummy4() = ()
type simpleRecord = {Name: string; Job: string; Wage: int}

[<Category("Defining Types");
  Title("More Basic Records");
  Description("Examples of cloning records using 'with' keyword and accessing record fields using . (dot).")>]
let RecordSample2() =
    let worker = { Name = "Bill"; Job = "Cashier"; Wage = 20 }
    
    let worker2 = { worker with Name = "John"; Wage = 15 }
    
    printfn "%s is a %s and makes $%d per hour" worker.Name worker.Job worker.Wage
    printfn "\n%s is a %s and makes $%d per hour" worker2.Name worker2.Job worker2.Wage
    
[<Support("MemberSample1")>]
let dummy5() = ()
type PointWithMembers = 
    { x: float; y: float}
    member p.VectorFromOrigin = { dx = p.x; dy = p.y }
    static member Origin = { x = 0.0; y = 0.0 }
    static member (+) ((p:PointWithMembers),(v:VectorWithMembers)) = 
        { x = p.x + v.dx; y = p.y + v.dy }
 
and VectorWithMembers = 
    { dx: float; dy: float}
    static member Zero = { dx = 0.0; dy = 0.0 }
    static member OneX = { dx = 1.0; dy = 0.0 }
    static member OneY = { dx = 0.0; dy = 1.0 }
    static member (+) ((v1:VectorWithMembers),(v2:VectorWithMembers)) = 
        { dx = v1.dx + v2.dx; dy = v1.dy + v2.dy }

[<Category("Defining Types");
  Title("Records with Members");
  Description("F# allows values to be associated with type names, i.e. the type name can be used as a container for 'members' of that type. These are treated the same way as .NET object model members.")>]
let MemberSample1() =
    printfn "Point.Origin = \n%A" PointWithMembers.Origin;
    printfn "Point.Origin + Vector.OneX = \n%A" (PointWithMembers.Origin + VectorWithMembers.OneX);
    printfn "Vector.OneX + Vector.OneY = \n%A" (VectorWithMembers.OneX + VectorWithMembers.OneY)

[<Support("Pattern1")>]
let dummy6() = ()
type car = {Make: string; Model: string; Year: int}

[<Category("Defining Types");
  Title("Records and Pattern Matching");
  Description("")>]
let Pattern1() =
    let car1 = {Make = "Porsche"; Model = "911"; Year = 2006}
    
    let car2 = {Make = "BMW"; Model = "335i"; Year = 2008}
    
    let car3 = {Make = "Subaru"; Model = "Impreza WRX"; Year = 2007}
    
    let allCars = [ car1; car2; car3 ]
    
    let subarus = 
        allCars
        |> List.filter
            (function
                | {Make = "Subaru"} -> true
                | _ -> false )
    
    printfn "subarus = %A" subarus

[<Support("MutualDefinition")>]
let dummy7() = ()
// define mutually recursive collections with the 'and' keyword
type Airport =
    { Name: string;
      Connections: connection list }
and connection =
    | Connection of Airport
    | None
    
[<Category("Defining Types");
  Title("Mutual Definitions");
  Description("Discriminated unions and records can be defined simultaneously to give mutually recursive collections of types.")>]
let MutualDefinition() =
    let LAX = { Name = "Los Angeles Intl Airport"; Connections = [connection.None] }
    let DTW = { Name = "Detroit Metro Wayne County"; Connections = [connection.None] }    
    let SEA = { Name = "Seattle-Tacoma Intl Airport"; 
                Connections = [connection.Connection DTW; connection.Connection LAX] }
             
    printfn "%A" LAX
    printfn "\n%A" DTW
    printfn "\n%A" SEA
    
[<Support("EnumSample")>]
let dummy8 () = ()
type FaceCard =
    | Jack = 11
    | Queen = 12
    | King = 13
    | Ace = 1

[<Category("Defining Types");
  Title("Enumerations");
  Description("Enums are lightweight types, similar to discriminated unions. Unlike discriminated unions, each data tag must be given a constant value of the same type")>]
let EnumSample() =
    let isAce card =
        match card with
        | FaceCard.Ace -> true
        | _ -> false
    
    printfn "Checking if a Jack is an Ace.....%A" (isAce FaceCard.Jack)

[<Support("structSample")>]
let dummy9() = ()
[<Struct>]
type StructBox (width: float, height: float, depth: float) =
    member box.Width = width
    member box.Height = height
    member box.Depth = depth
    
type StructVector2D (dx: float, dy: float) =
    struct     
        member v.DX = dx
        member v.DY = dy
        member v.Length = sqrt (v.DX * v.DX + v.DY * v.DY)
    end
    
[<Category("Defining Types");
  Title("Structs");
  Description("Structs are lightweight data structures, stored on the Stack. They use value equality comparison.")>]
let structSample() =
    // a simple struct - must use [<Struct>] attribute tag or 'struct' 'end' keywords to define as struct
    // [<Struct>] 
    // type StructBox (width: float, height: float, depth: float) =
    //     member box.Width = width
    //     member box.Height = height
    //     member box.Depth = depth
        
    // type StructVector2D (dx: float, dy: float) =
    //     struct     
    //         member v.DX = dx
    //         member v.DY = dy
    //         member v.Length = sqrt (v.DX * v.DX + v.DY * v.DY)
    //     end
    
    let b1 = StructBox(2.0, 3.0, 6.0)
    
    let v1 = StructVector2D(3.0, 4.0)
    
    let v2 = StructVector2D(3.0, 4.0)
    
    printfn "b1.Width = %A" b1.Width
    printfn"\nv1.Length = %A" v1.Length
    printfn"\nv1 = v2?\t%A" (v1=v2)
    
[<Support("ClassesSupport")>]
let dummy10() = ()
type Circle (r: float) =
    let area = System.Math.PI * r * r
    do printfn "Initialized a circle with radius %f\n" r
    
    member this.R = r
    member this.Area = area
    
    new () = new Circle(1.0)
    
    new(text: string) = 
        if text = null then
            raise <| new System.Exception("text")
        
        let (successR, r) = System.Double.TryParse(text)
        if not successR then
            raise <| new System.Exception("text")
        new Circle(r: float)
        
          
[<Category("Defining Types");
  Title("Constructing Classes");
  Description("Classes are custom types containing data (known as 'fields'), methods, and properties ( methods without parameters). Classes are reference types and are stored on the Heap. Classes must be constructed and instantiated to use. Unlike C#, no default constructor is provided, one must be defined.")>]
let ClassesSupport() =
    let c1 = new Circle()
    let area1 = c1.Area

    let c2 = new Circle(6.0)
    let area2 = c2.Area

    let c3 = new Circle("3")
    let area3 = c3.Area
    
    printfn "area1 = %f\n" area1
    printfn "area2 = %f\n" area2
    printfn "area3 = %f\n" area3

[<Support("ClassesSupport2")>]
let dummy11() = ()
type Circle2 (r : float) =    
    let mutable radius = r
    let area = System.Math.PI * r * r
    
    // Read/Write property with get and set
    member c.R with get() = radius and set(inp) = radius <- inp 
    // Read only property with get
    member c.Area with get() = area
    
    new () = Circle2(1.0)
    
    new(text: string) = 
        if text = null then
            raise <| new System.Exception("text")
        
        let (successR, r) = System.Double.TryParse(text)
        if not successR then
            raise <| new System.Exception("text")
        new Circle2(r: float)
    
    // method with signature:    member CalculateCircumference : unit -> float
    member c.CalculateCircumference() = System.Math.PI * 2.0 * c.R

[<Category("Defining Types");
  Title("Classes with methods and properties");
  Description("Classes can contain methods of general form and properties.")>]  
let ClassesSupport2() =
    let c1 = Circle2(2.0)
    
    printfn "c1.Area = %f\tc1.R = %f\n" c1.Area c1.R
    c1.R <- 5.0 
    printfn "c1.Area = %f\tc1.R = %f\n" c1.Area c1.R
    
    c1.CalculateCircumference()
    
//--------------------------------------------------------

[<Category("Disposal");
  Title("Basic Use Bindings");
  Description("The 'use' binding indicates that the IDisposable.Dispose method should be called on the object at the end of its lexical scope. In this case it closes the file deterministically.")>]
let DisposeSample1() =
    System.IO.File.WriteAllLines(@"test.txt", [| "This is a test file."; 
                                                 "It is easy to read." |]);

    use sr = System.IO.File.OpenText @"test.txt"
    let line1 = sr.ReadLine() 
    let line2 = sr.ReadLine() 
    printfn "line1 = %s" line1
    printfn "line2 = %s" line2

//--------------------------------------------------------
  
[<Category("Input/Output");
  Title("Read Two Lines, .NET-style");
  Description("Line-directed text file input using .NET Stream/StreamReader/StreamWriter abstractions.")>]
let LineDirectedInputSample2() = 

    // Write a test file
    System.IO.File.WriteAllLines(@"test.txt", [| "This is a test file."; 
                                                 "It is easy to read." |]);

    // Now read it.  We use  to ensure the file is closed even if an exception occurs
    // during reading.  
    let line1,line2 = 
        use sr = System.IO.File.OpenText @"test.txt"
        let line1 = sr.ReadLine() 
        let line2 = sr.ReadLine() 
        (line1,line2)
    printfn "line1=%s\nline2=%s" line1 line2

[<Category("Input/Output");
  Title("Read All Lines, .NET-style");
  Description("Line-directed text file input using .NET Stream/StreamReader/StreamWriter abstractions.")>]
let LineDirectedInputSample3() = 

    // Write a test file
    System.IO.File.WriteAllLines(@"test.txt", [| "This is a test file."; 
                                                 "It is easy to read." |]);
    // Now read it 
    let lines = System.IO.File.ReadAllLines @"test.txt"  
    printfn "%s" (sprintf "%A" lines)
     
[<Category("Input/Output");
  Title("Read Entire File, .NET-style");
  Description("Read an entire text file as a string using .NET I/O utilities and abstractions.")>]
let EntireFileInputSample2() = 

    // Write a test file
    System.IO.File.WriteAllLines(@"test.txt", [| "This is a test file."; 
                                                 "It is easy to read." |]);
    // Now read it 
    let res = System.IO.File.ReadAllText(@"test.txt")  
    printfn "%s" res
        

[<Category("Input/Output");
  Title("Read Entire CSV File, .NET-style");
  Description("Read an entire text file as a string using .NET I/O utilities and abstractions.")>]
let ReadCSVFile1() = 

    // Write a test file
    System.IO.File.WriteAllLines(@"test.csv", [| "Desmond, Barrow, Market Place, 2"; 
                                                 "Molly, Singer, Band, 12" |]);
    // Now read it 
    let linesSplitIntoWords = 
        System.IO.File.ReadAllLines(@"test.csv")
        |> Array.map (fun line -> line.Split [|',';' ';'\t'|])
    printfn "%A" linesSplitIntoWords