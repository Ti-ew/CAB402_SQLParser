module Compare

open Microsoft.VisualStudio.TestTools.UnitTesting
open RuntimeTypes

let groupsToLists group = 
    group |> Seq.map Seq.toList |> Seq.toList

let compareRelations expected actual =
    Assert.AreEqual(expected.columnsInfo, actual.columnsInfo)

    if (actual.rows |> Seq.toList) <> (expected.rows |> Seq.toList) then
        printf "Expected:"
        printfn "%s" (IO.RelationToString expected)
        printf "Actual:"
        printf "%s" (IO.RelationToString actual)
        Assert.Fail "Rows are not the same"
            
    match expected.groups, actual.groups with
    | Some(expectedGroups), Some(actualGroups) ->
        if (expectedGroups |> groupsToLists) <> (actualGroups |> groupsToLists) then
            printf "Expected:"
            printfn "%s" (IO.RelationToString expected)
            printf "Actual:"
            printf "%s" (IO.RelationToString actual)
            Assert.Fail "Groups are not the same"
    | None, Some(_) ->
        printf "Expected:"
        printfn "%s" (IO.RelationToString expected)
        printf "Actual:"
        printf "%s" (IO.RelationToString actual)
        Assert.Fail "Actual has groups but expected does not"
    | Some(_), None ->
        printf "Expected:"
        printfn "%s" (IO.RelationToString expected)
        printf "Actual:"
        printf "%s" (IO.RelationToString actual)
        Assert.Fail "Expected has groups but actual does not"
    | None, None ->
        ()