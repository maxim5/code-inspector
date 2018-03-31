module Problem17

open System.Linq
open System.Collections.Generic
open Xunit

// How many letters are used to write out the numbers from 1 to 1000?

let singleDigits = 
    [(0, "");
     (1, "one");
     (2, "two");
     (3, "three");
     (4, "four");
     (5, "five");
     (6, "six");
     (7, "seven");
     (8, "eight");
     (9, "nine")].ToDictionary(fst, snd)

let teens = 
    [(10, "ten");
     (11, "eleven");
     (12, "twelve");
     (13, "thirteen");
     (14, "fourteen");
     (15, "fifteen");
     (16, "sixteen");
     (17, "seventeen");
     (18, "eighteen");
     (19, "nineteen")].ToDictionary(fst, snd)

let multsOfTen =
    [(20, "twenty");
     (30, "thirty");
     (40, "forty");
     (50, "fifty");
     (60, "sixty");
     (70, "seventy");
     (80, "eighty");
     (90, "ninety")].ToDictionary(fst, snd)

let rec spellNumber n =
    if n < 10 then
        singleDigits.[n]
    elif n >= 10 && n < 20 then
        teens.[n]
    elif n >= 20 && n < 100 then
        multsOfTen.[(n / 10) * 10] + " " + singleDigits.[n % 10]
    elif n >= 100 && n < 1000 then
        singleDigits.[n / 100] + " hundred " + (if n % 100 = 0 then "" else " and ") + (spellNumber (n % 100))
    else  // just a hack for this problem
        "one thousand"

let run = 
    [1..1000]
    |> Seq.map spellNumber
    |> Seq.reduce (+)
    |> Seq.filter (fun c -> c <> ' ')
    |> Seq.length

[<Fact>]
let testNinetyNine () =
    Assert.Equal("ninety nine", spellNumber 99)

let testNineHundredNinetyNine () =
    Assert.Equal("nine hundred and ninety nine", spellNumber 999)
