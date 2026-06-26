module SyncBackup.Tests.Domain.Scan

open SyncBackup.Tests.Properties.CustomGenerators
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Scan

module ``buildScanResult should`` =
    let isAsExpected expected result =
        let result = Result.map Set result
        let expected = Set expected
        test <@ result = Ok expected @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``accepts when no element scanned`` rules tracked =
        let result = buildScanResult rules tracked []
        test <@ Result.isOk result @>
        test <@ Result.map (List.forall (fun diff -> diff.Diff = RemovedFromRepository)) result = Ok true @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return all paths as added without rule when no tracked neither rules`` contents =
        contents <> [] ==> lazy
        let result = buildScanResult [] [] contents
        let expected = contents |> List.map (fun content -> { SyncRule = NoRule; Path = content.Path; Diff = AddedToRepository })
        result |> isAsExpected expected

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return path with existing rule`` content syncRule =
        let rule = { SyncRule = syncRule; Path = content.Path }
        let result = buildScanResult [rule] [] [content]
        result |> isAsExpected [ScanResult.build AddedToRepository rule]

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return only delta between scan and tracked elements`` (paths: RelativePath list) =
        let contents = List.distinct paths |> List.map (fun path -> { Path = path; LastWriteTime = None })
        List.length contents > 3 ==> lazy
        let added, removed, common =
            match contents with
            | added::removed::common -> added, removed, common
            | _ -> failwith "unexpected pattern"

        let result = buildScanResult [] (removed::common) (added::common)
        let expected = [
            { SyncRule = NoRule; Path = removed.Path; Diff = RemovedFromRepository }
            { SyncRule = NoRule; Path = added.Path; Diff = AddedToRepository }
        ]
        result |> isAsExpected expected

    [<Fact>]
    let ``return updated content`` () =
        let lastWriteTime = System.DateTime(2024, 08, 11, 15, 27, 30)
        let dir = { Path = { Value = "dir"; Type = Source; ContentType = ContentType.Directory }; LastWriteTime = None }
        let content1 = { Path = { Value = "dir\\file1"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }
        let content2 = { Path = { Value = "dir\\file2"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }
        let content3 = { Path = { Value = "dir\\file3"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }
        let content4 = { Path = { Value = "dir\\file4"; Type = Source; ContentType = ContentType.File }; LastWriteTime = None }
        let content5 = { Path = { Value = "dir\\file5"; Type = Source; ContentType = ContentType.File }; LastWriteTime = None }
        let content6 = { Path = { Value = "dir\\file6"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some (lastWriteTime.AddDays 1) }

        let trackedContent = [dir; content1; content2; content3; content4; content5; content6]
        let scannedContent = [
            dir
            { content1 with LastWriteTime = Some (lastWriteTime.AddDays 1) }
            { content2 with LastWriteTime = Some lastWriteTime }
            { content3 with LastWriteTime = None }
            { content4 with LastWriteTime = Some lastWriteTime }
            { content5 with LastWriteTime = None }
            { content6 with LastWriteTime = Some lastWriteTime }
        ]
        let result = buildScanResult [] trackedContent scannedContent

        let expected = [
            { SyncRule = NoRule; Path = content1.Path; Diff = Updated }
        ]
        result |> isAsExpected expected

    [<Fact>]
    let ``include rule reminder for the user when one parent has rule but remains unchanged`` () =
        let lastWriteTime = System.DateTime(2024, 08, 11, 15, 27, 30)
        let dir = { Path = { Value = "dir"; Type = Source; ContentType = ContentType.Directory }; LastWriteTime = None }
        let subDir1 = { Path = { Value = "dir\\subdir1"; Type = Source; ContentType = ContentType.Directory }; LastWriteTime = None }
        let subDir2 = { Path = { Value = "dir\\subdir1\\subdir2"; Type = Source; ContentType = ContentType.Directory }; LastWriteTime = None }
        let subDir3 = { Path = { Value = "dir\\subdir1\\subdir2\\subdir3"; Type = Source; ContentType = ContentType.Directory }; LastWriteTime = None }
        let content1 = { Path = { Value = "dir\\file1"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }
        let content2 = { Path = { Value = "dir\\subdir1\\file2"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }
        let content3 = { Path = { Value = "dir\\subdir1\\subdir2\\file3"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }
        let content4 = { Path = { Value = "dir\\subdir1\\subdir2\\subdir3\\file4"; Type = Source; ContentType = ContentType.File }; LastWriteTime = Some lastWriteTime }

        let trackedContent = [dir; subDir1; subDir2; subDir3; content1]
        let scannedContent = [dir; subDir1; subDir2; subDir3; content1; content2; content3; content4]
        let rules = [
            { Path = dir.Path; SyncRule = Include }
            { Path = subDir3.Path; SyncRule = Exclude }
        ]
        let result = buildScanResult rules trackedContent scannedContent

        let expected = [
            { SyncRule = Include; Path = dir.Path; Diff = RuleReminder }
            { SyncRule = Include; Path = subDir1.Path; Diff = RuleReminder } // Inherited from parent
            { SyncRule = NoRule; Path = content2.Path; Diff = AddedToRepository }
            { SyncRule = Include; Path = subDir2.Path; Diff = RuleReminder } // Inherited from parent
            { SyncRule = NoRule; Path = content3.Path; Diff = AddedToRepository }
            { SyncRule = Exclude; Path = subDir3.Path; Diff = RuleReminder }
            { SyncRule = NoRule; Path = content4.Path; Diff = AddedToRepository }
        ]
        result |> isAsExpected expected

module ``defineTrackedElements should`` =
    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``not remove elements that are not in scan result`` tracked =
        let result = defineTrackedElements tracked []
        test <@ Set result = Set tracked @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``add elements that are flag as added`` tracked element =
        let result = defineTrackedElements tracked [ScanResult.build AddedToRepository element]
        test <@ Set  result = Set (tracked@[element.Path]) @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``removed elements that are flag as removed`` tracked (rule: Rule) =
        not (tracked |> List.exists ((=) rule.Path)) ==> lazy
        let result = defineTrackedElements (tracked@[rule.Path]) [ScanResult.build RemovedFromRepository rule]
        test <@ Set result = Set tracked @>
