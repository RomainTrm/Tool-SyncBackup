module SyncBackup.Tests.Domain.Sync

open System
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Domain.Sync

let lastWriteTime = DateTime(2024, 08, 10, 10, 43, 30)

let d1 = { Path = { Type = Source; Value = "d1"; ContentType = Directory }; LastWriteTime = None }
let d2 = { Path = { Type = Source; Value = "d2"; ContentType = Directory }; LastWriteTime = None }
let d3 = { Path = { Type = Source; Value = "d3"; ContentType = Directory }; LastWriteTime = None }
let d1s1 = { Path = { Type = Source; Value = "d1\\s1"; ContentType = Directory }; LastWriteTime = None }
let d1s2 = { Path = { Type = Source; Value = "d1\\s2"; ContentType = Directory }; LastWriteTime = None }
let d1s1f1 = { Path = { Type = Source; Value = "d1\\s1\\f1"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d1s1f2 = { Path = { Type = Source; Value = "d1\\s1\\f2"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d1s2f1 = { Path = { Type = Source; Value = "d1\\s2\\f1"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f1 = { Path = { Type = Source; Value = "d2\\f1"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f2 = { Path = { Type = Source; Value = "d2\\f2"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f3 = { Path = { Type = Source; Value = "d2\\f3"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f4 = { Path = { Type = Source; Value = "d2\\f4"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d2f5 = { Path = { Type = Source; Value = "d2\\f5"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d3f1 = { Path = { Type = Source; Value = "d3\\f1"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d3f2 = { Path = { Type = Source; Value = "d3\\f2"; ContentType = File }; LastWriteTime = Some lastWriteTime }
let d3f3 = { Path = { Type = Source; Value = "d3\\f3"; ContentType = File }; LastWriteTime = Some lastWriteTime }

let rnd = Random();
let randomizeOrder _ _ = rnd.Next ()

module ``synchronize should`` =
    [<Fact>]
    let ``compute synchronize instructions when empty backup`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let sourceRules: Rule list = [
            { Path = d2.Path; SyncRule = SyncRules.Include }
            { Path = d3.Path; SyncRule = SyncRules.Exclude }
            { Path = d2f2.Path; SyncRule = SyncRules.Include }
            { Path = d2f3.Path; SyncRule = SyncRules.Exclude }
            { Path = d3f2.Path; SyncRule = SyncRules.Include }
            { Path = d3f3.Path; SyncRule = SyncRules.Exclude }
        ]

        let result = Synchronize.run sourceItems sourceRules [] []

        let expected = [
            Add d1.Path
            Add d2.Path
            Add d2f1.Path
            Add d2f2.Path
            Add d3.Path
            Add d3f2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup`` () =
        let sourceItems = [
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let backupRules = [
            { Path = d2f1.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3.Path; SyncRule = SyncRules.NotSave }
            { Path = d3f1.Path; SyncRule = SyncRules.NotDelete }
            { Path = d3f2.Path; SyncRule = SyncRules.NotDelete }
            { Path = d3f3.Path; SyncRule = SyncRules.NotDelete }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        let expected = [
            Delete d1.Path
            Replace d2f1.Path
            Replace d2f2.Path
            Delete d2f3.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup (conflicting rules)`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d2f4
        ]

        let sourceRules: Rule list = [
            { Path = d1.Path; SyncRule = SyncRules.Include }
            { Path = d2.Path; SyncRule = SyncRules.Include }
            { Path = d2f1.Path; SyncRule = SyncRules.Exclude }
            { Path = d2f2.Path; SyncRule = SyncRules.Exclude }
            { Path = d2f3.Path; SyncRule = SyncRules.Exclude }
            { Path = d2f4.Path; SyncRule = SyncRules.Exclude }
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d2f4
            d3f1
            d3f2
        ]

        let backupRules = [
            { Path = d1.Path; SyncRule = SyncRules.NotDelete }
            { Path = d2f2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3.Path; SyncRule = SyncRules.NotDelete }
            { Path = d2f4.Path; SyncRule = SyncRules.NotSave }
            { Path = d3f1.Path; SyncRule = SyncRules.NotSave }
            { Path = d3f2.Path; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems sourceRules backupItems backupRules

        let expected = [
            Delete d2f1.Path
            Delete d2f2.Path
            Delete d2f4.Path
            Delete d3f1.Path
            Delete d3f2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when source repository contains aliases`` () =
        let sourceItems = [
            { d2 with Path.Type = Alias }
            { d2f1 with Path.Type = Alias }
            { d2f2 with Path.Type = Alias }
            { d2f3 with Path.Type = Alias }
        ]

        let backupItems = [
            d2
            d2f1
            d2f2
            d2f3
        ]

        let backupRules = [
            { Path = d2f1.Path; SyncRule = SyncRules.NotDelete }
            { Path = d2f2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3.Path; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        let expected = [
            Replace { d2f2.Path with Type = Alias }
            Replace { d2f3.Path with Type = Alias }
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions should only replace files`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupRules = [
            { Path = d1.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f1.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3.Path; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        let expected = [
            Replace d2f1.Path
            Replace d2f2.Path
            Replace d2f3.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute add order`` () =
        let sourceItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let result = Synchronize.run sourceItems [] [] []

        let expected = [
            Add d1.Path
            Add d1s1.Path
            Add d1s1f1.Path
            Add d1s1f2.Path
            Add d1s2.Path
            Add d1s2f1.Path
            Add d2.Path
            Add d2f1.Path
            Add d2f2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute delete order`` () =
        let backupItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let result = Synchronize.run [] [] backupItems []

        let expected = [
            Delete d1s1f1.Path
            Delete d1s1f2.Path
            Delete d1s1.Path
            Delete d1s2f1.Path
            Delete d1s2.Path
            Delete d1.Path
            Delete d2f1.Path
            Delete d2f2.Path
            Delete d2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``not delete directory if keeping children`` () =
        let backupItems = [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let backupRules = [
            { Path = d1s1f2.Path; SyncRule = SyncRules.NotDelete }
            { Path = d2.Path; SyncRule = SyncRules.NotDelete }
        ]

        let result = Synchronize.run [] [] backupItems backupRules

        let expected = [
            Delete d1s1f1.Path
            Delete d1s2f1.Path
            Delete d1s2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``not delete element even if excluded`` () =
        let sourceItems = [
            d2
            d2f1
        ]

        let sourceRules = [
            { Path = d2f1.Path; SyncRule = SyncRules.Exclude }
        ]

        let backupItems = [
            d2
            d2f1
        ]

        let backupRules = [
            { Path = d2f1.Path; SyncRule = SyncRules.NotDelete }
        ]

        let result = Synchronize.run sourceItems sourceRules backupItems backupRules

        test <@ result = Ok [] @>

    [<Fact>]
    let ``apply 'ignore' rule to children`` () =
        let sourceItems = [
            d1
            d2
            d2f1
            d2f2
            d3
            d3f1
            d3f2
        ]

        let sourceRules = [
            { Path = d3f1.Path; SyncRule = SyncRules.Exclude }
            { Path = d3f2.Path; SyncRule = SyncRules.Include }
        ]

        let backupRules = List.sortWith randomizeOrder [
            { Path = d1.Path; SyncRule = SyncRules.NotSave }
            { Path = d2.Path; SyncRule = SyncRules.NotSave }
            { Path = d3.Path; SyncRule = SyncRules.NotSave }
        ]

        let result = Synchronize.run sourceItems sourceRules [] backupRules

        test <@ result = Ok [] @>

    [<Fact>]
    let ``not delete root directory when excluded but another rule for a file inside override it`` () =
        let sourceItems = [
            d3
            d3f1
            d3f2
            d3f3
        ]

        let backupItems = [
            d3
            d3f1
        ]

        let backupRules = List.sortWith randomizeOrder [
            { Path = d3.Path; SyncRule = SyncRules.NotSave }
            { Path = d3f1.Path; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Synchronize.run sourceItems [] backupItems backupRules

        test <@ result = Ok [
            Replace d3f1.Path
        ] @>

    [<Fact>]
    let ``replace file when updated`` () =
        let sourceItems = [
            d2
            { d2f1 with LastWriteTime = Some (lastWriteTime.AddDays 1) }
            { d2f2 with LastWriteTime = Some lastWriteTime }
            { d2f3 with LastWriteTime = None }
            { d2f4 with LastWriteTime = Some lastWriteTime }
            { d2f5 with LastWriteTime = Some lastWriteTime }
        ]

        let backupItems = [
            d2
            { d2f1 with LastWriteTime = Some lastWriteTime }
            { d2f2 with LastWriteTime = Some lastWriteTime }
            { d2f3 with LastWriteTime = Some lastWriteTime }
            { d2f4 with LastWriteTime = None }
            { d2f5 with LastWriteTime = Some (lastWriteTime.AddDays 1) }
        ]

        let result = Synchronize.run sourceItems [] backupItems []

        test <@ result = Ok [
            Replace d2f1.Path
        ] @>

module ``replicate should`` =
    [<Fact>]
    let ``compute synchronize instructions when empty backup`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let result = Replicate.run [] sourceItems []

        let expected = [
            Add d1.Path
            Add d2.Path
            Add d2f1.Path
            Add d2f2.Path
            Add d2f3.Path
            Add d3.Path
            Add d3f1.Path
            Add d3f2.Path
            Add d3f3.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions when not empty backup`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
            d3f1
            d3f2
            d3f3
        ]

        let backupItems = [
            d1
            d3
            d3f1
            d3f2
            d3f3
        ]

        let rules = [
            { Path = d3f1.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3f2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3f3.Path; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Replicate.run rules sourceItems backupItems

        let expected = [
            Add d2.Path
            Add d2f1.Path
            Add d2f2.Path
            Add d2f3.Path
            Replace d3f1.Path
            Replace d3f2.Path
            Replace d3f3.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute synchronize instructions should only replace files`` () =
        let sourceItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let backupItems = [
            d1
            d2
            d3
            d2f1
            d2f2
            d2f3
        ]

        let rules = [
            { Path = d1.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d3.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f1.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f2.Path; SyncRule = SyncRules.AlwaysReplace }
            { Path = d2f3.Path; SyncRule = SyncRules.AlwaysReplace }
        ]

        let result = Replicate.run rules sourceItems backupItems

        let expected = [
            Replace d2f1.Path
            Replace d2f2.Path
            Replace d2f3.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute add order`` () =
        let sourceItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let result = Replicate.run [] sourceItems []

        let expected = [
            Add d1.Path
            Add d1s1.Path
            Add d1s1f1.Path
            Add d1s1f2.Path
            Add d1s2.Path
            Add d1s2f1.Path
            Add d2.Path
            Add d2f1.Path
            Add d2f2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``compute delete order`` () =
        let backupItems = List.sortWith randomizeOrder [
            d1
            d1s1
            d1s1f1
            d1s1f2
            d1s2
            d1s2f1
            d2
            d2f1
            d2f2
        ]

        let rules = [
            { Path = d2f1.Path; SyncRule = NotSave }
            { Path = d2f2.Path; SyncRule = AlwaysReplace }
        ]

        let result = Replicate.run rules [] backupItems

        let expected = [
            Delete d1s1f1.Path
            Delete d1s1f2.Path
            Delete d1s1.Path
            Delete d1s2f1.Path
            Delete d1s2.Path
            Delete d1.Path
            Delete d2f1.Path
            Delete d2f2.Path
            Delete d2.Path
        ]
        test <@ result = Ok expected @>

    [<Fact>]
    let ``apply 'preserve' rule`` () =
        let backupItems = [
            d2
            d2f1
            d2f2
        ]

        let rules = [
            { Path = d2.Path; SyncRule = NotDelete }
        ]
        let result = Replicate.run rules [] backupItems

        test <@ result = Ok [] @>

    [<Fact>]
    let ``replace file when updated`` () =
        let sourceItems = [
            d2
            { d2f1 with LastWriteTime = Some (lastWriteTime.AddDays 1) }
            { d2f2 with LastWriteTime = Some lastWriteTime }
            { d2f3 with LastWriteTime = None }
            { d2f4 with LastWriteTime = Some lastWriteTime }
            { d2f5 with LastWriteTime = Some lastWriteTime }
        ]

        let backupItems = [
            d2
            { d2f1 with LastWriteTime = Some lastWriteTime }
            { d2f2 with LastWriteTime = Some lastWriteTime }
            { d2f3 with LastWriteTime = Some lastWriteTime }
            { d2f4 with LastWriteTime = None }
            { d2f5 with LastWriteTime = Some (lastWriteTime.AddDays 1) }
        ]

        let result = Replicate.run [] sourceItems backupItems

        test <@ result = Ok [
            Replace d2f1.Path
        ] @>
