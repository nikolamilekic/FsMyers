module FsMyers.Tests

open Expecto
open FsCheck
open Swensen.Unquote
open System.Linq

let stringToMyersList (s : string) =
    let charArray = s.ToCharArray()
    { new IMyersList<_> with member __.Count = charArray.Length
                             member __.Item with get(x) = charArray.[x] }
let getStringDifferences a b = (stringToMyersList a, stringToMyersList b)
                               |> Myers.Default.GetDiffs
                               |> Async.RunSynchronously

let insert i e (l : ResizeArray<_>) = l.Insert(i, e); l
let delete i (l : ResizeArray<_>) = l.RemoveAt i; l

let testApplication (a : string) b differences =
    Myers.ApplyDifferences(insert, delete, differences, (a.ToList())).ToArray()
    |> System.String = b

let testCountAndApplication a b expectedCount =
    let title = sprintf "'%s' -> '%s'" a b
    let test() =
        let differences = getStringDifferences a b
        let applicationResult = testApplication a b differences
        test <@ testApplication a b differences &&
                Seq.length differences = expectedCount @>
    title, test

[<Tests>]
let knownSamples =
    testList "Known samples" [
        testCase <|| testCountAndApplication "abcabba" "cbabac" 5
        testCase <|| testCountAndApplication "nokkolla" "nikola" 4
        testCase <|| testCountAndApplication "a" "b" 2
        testCase <|| testCountAndApplication "a" "a" 0
        testCase <|| testCountAndApplication "aa" "aa" 0
        testCase <|| testCountAndApplication "aaa" "aaa" 0
        testCase <|| testCountAndApplication "a" "ab" 1
        testCase <|| testCountAndApplication "ab" "a" 1
        testCase <|| testCountAndApplication "aaa" "a" 2
        testCase <|| testCountAndApplication "bab" "a" 2
        testCase <|| testCountAndApplication "a" "aaa" 2
        testCase <|| testCountAndApplication "ab" "bb" 2
        testCase <|| testCountAndApplication "aa" "bb" 4
        testCase <|| testCountAndApplication "" "aaa" 3
        testCase <|| testCountAndApplication "aaa" "" 3
        testCase <|| testCountAndApplication "abc" "cba" 4
        testCase <|| testCountAndApplication "abcd" "cba" 5
        testCase <|| testCountAndApplication "abcde" "cba" 6
        testCase <|| testCountAndApplication "abcdef" "cba" 7
        testCase <|| testCountAndApplication "cba" "abc" 4
        testCase <|| testCountAndApplication "cba" "abcd" 5
        testCase <|| testCountAndApplication "abcdefg" "cbag" 7
        testCase <|| testCountAndApplication "00abcdefgg" "00cbagg" 7
        testCase <|| testCountAndApplication "00abcdxyzefgg" "00cbxyzagg" 7
        testCase <|| testCountAndApplication "00abcdxyzefgg" "00cbxyzabcdagg" 11
        testCase <|| testCountAndApplication "00aabcdxyzefgg" "00abcdxxyzegg" 3
        testCase <|| testCountAndApplication "00aabcdxyzefgg" "00abcd" 8
        testCase <|| testCountAndApplication "00abcd" "00aabcdxyzefgg" 8
    ]

[<Tests>]
let random =
    testProperty "Applying diffs with random strings"
        (fun (NonNull a, NonNull b) -> getStringDifferences a b
                                       |> testApplication a b)
