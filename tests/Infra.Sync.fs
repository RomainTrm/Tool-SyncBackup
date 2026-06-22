module SyncBackup.Tests.Infra.Sync

open System.IO
open SyncBackup.Domain.Dsl
open Xunit
open Swensen.Unquote
open SyncBackup.Tests.Infra

open SyncBackup.Domain.Sync
open SyncBackup.Infra
open SyncBackup.Infra.Sync

module InstructionsFile =
    module ``save should`` =
        [<Fact>]
        let ``no instructions should display "up to date"`` () =
            let uniqueTestDirectory = "test-57e4353c-f89a-4081-85e7-56077fdc147a"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = InstructionsFile.save path "whatever" []
            test <@ result = Ok () @>

            let fileContent = Dsl.getSyncInstructionsFilePath path |> System.IO.File.ReadAllLines
            test <@ fileContent |> Seq.contains "# No change, your backup is up to date."  @>

        [<Fact>]
        let ``persist instructions`` () =
            let uniqueTestDirectory = "test-1871011a-9f98-4058-aa6f-171de28c383d"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = InstructionsFile.save path "whatever" [
                SyncInstruction.Add { Value = "path1"; Type = Alias; ContentType = ContentType.File }
                SyncInstruction.Replace { Value = "path2"; Type = Source; ContentType = ContentType.Directory }
                SyncInstruction.Delete { Value = "path3"; Type = Source; ContentType = ContentType.File }
            ]
            test <@ result = Ok () @>

            let fileContent = Dsl.getSyncInstructionsFilePath path |> System.IO.File.ReadAllLines
            let expected = [|
                "- Add: file::\"*path1\""
                "- Replace: dir::\"path2\""
                "- Delete: file::\"path3\""
            |]
            test <@ (Set fileContent) |> Set.isSubset (Set expected)  @>

    module ``areInstructionsAccepted should`` =
        [<Fact>]
        let ``return error if file is missing`` () =
            let uniqueTestDirectory = "test-eb63cf53-3de0-4402-b4a3-5fd0c727f879"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ Result.isError result @>

        [<Fact>]
        let ``return false if accept is commented`` () =
            let uniqueTestDirectory = "test-9e2d72b5-629d-4761-ab36-12ed1a241f83"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let _ = InstructionsFile.save path "whatever" []

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ result = Ok false @>

        [<Theory>]
        [<InlineData("  Accept")>]
        [<InlineData("Accept")>]
        [<InlineData("    Accept  ")>]
        let ``return true if accept is uncommented`` accept =
            let uniqueTestDirectory = "test-cf485f28-f85f-453d-83b7-f46514edfccd"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let _ = InstructionsFile.save path "whatever" []
            let filePath = Dsl.getSyncInstructionsFilePath path
            let fileContent = System.IO.File.ReadAllText filePath
            let acceptedFileContent = fileContent.Replace("# Accept", accept)
            System.IO.File.WriteAllText (filePath, acceptedFileContent)

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ result = Ok true @>

        [<Fact>]
        let ``should not return false positive on instruction with Accept in path`` () =
            let uniqueTestDirectory = "test-3d617642-78f7-4a96-8386-487c92443c1f"
            let path = TestHelpers.setupConfigDirectoryTest uniqueTestDirectory

            let _ = InstructionsFile.save path "whatever" [
                SyncInstruction.Add { ContentType = File; Value = "Accept.pdf"; Type = PathType.Source  }
            ]
            let filePath = Dsl.getSyncInstructionsFilePath path
            let fileContent = System.IO.File.ReadAllText filePath
            let acceptedFileContent = fileContent.Replace("# Accept", "")
            System.IO.File.WriteAllText (filePath, acceptedFileContent)

            let result = InstructionsFile.areInstructionsAccepted path
            test <@ result = Ok false @>

module Process =
    let setupSyncDirectoryTest uniqueTestDirectory =
        let path = TestHelpers.testDirectoryPath uniqueTestDirectory
        TestHelpers.cleanupTests path
        TestHelpers.createDirectory [|uniqueTestDirectory; "source"|]
        TestHelpers.createDirectory [|uniqueTestDirectory; "backup"|]
        (Path.Combine (path, "source")), (Path.Combine (path, "backup"))

    module ``run Add should`` =
        [<Fact>]
        let ``add new file`` () =
            let uniqueTestDirectory = "test-9d3d28c6-4cbc-4371-9678-6ac9665717be"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createFile [|sourcePath; "file"|]

            test <@ File.Exists (Path.Combine (backupPath, "file")) = false @>

            [
                Add { Value = "file"; Type = Source; ContentType = File }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) []
            |> ignore

            test <@ File.Exists (Path.Combine (backupPath, "file")) = true @>

        [<Fact>]
        let ``add new directory`` () =
            let uniqueTestDirectory = "test-5cd28061-7c58-4bf5-807f-917689ef3e4d"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createDirectory [|sourcePath; "directory"|]

            test <@ Directory.Exists (Path.Combine (backupPath, "directory")) = false @>

            [
                Add { Value = "directory"; Type = Source; ContentType = Directory }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) []
            |> ignore

            test <@ Directory.Exists (Path.Combine (backupPath, "directory")) = true @>

        [<Fact>]
        let ``add new file from alias source`` () =
            let uniqueTestDirectory = "test-b6ad5ea2-ff08-4e97-b43f-72f15a0f6be5"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createDirectory [|backupPath; "alias"|]
            TestHelpers.createFile [|sourcePath; "file"|]

            test <@ File.Exists (Path.Combine (backupPath, "alias", "file")) = false @>

            [
                Add { Value = "alias\\file"; Type = Alias; ContentType = File }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) [
                { Name = "alias"; Path = sourcePath }
            ]
            |> ignore

            test <@ File.Exists (Path.Combine (backupPath, "alias", "file")) = true @>

    module ``run Replace should`` =
        [<Fact>]
        let ``replace existing file`` () =
            let uniqueTestDirectory = "test-22998e7a-8766-44c0-8acc-f914bea8c33c"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createFile' [|sourcePath; "file"|] "sourceContent"
            TestHelpers.createFile' [|backupPath; "file"|] "backupContent"

            test <@ File.ReadAllText (Path.Combine (backupPath, "file")) = "backupContent" @>

            [
                Replace { Value = "file"; Type = Source; ContentType = File }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) []
            |> ignore

            test <@ File.ReadAllText (Path.Combine (backupPath, "file")) = "sourceContent" @>

        [<Fact>]
        let ``replace existing file from alias source`` () =
            let uniqueTestDirectory = "test-4dda5a57-d398-4c7e-8fcb-7fb498b088d8"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createFile' [|sourcePath; "file"|] "sourceContent"
            TestHelpers.createDirectory [|backupPath; "alias"|]
            TestHelpers.createFile' [|backupPath; "alias"; "file"|] "backupContent"

            test <@ File.ReadAllText (Path.Combine (backupPath, "alias", "file")) = "backupContent" @>

            [
                Replace { Value = "alias\\file"; Type = Alias; ContentType = File }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) [
                { Name = "alias"; Path = sourcePath }
            ]
            |> ignore

            test <@ File.ReadAllText (Path.Combine (backupPath, "alias", "file")) = "sourceContent" @>

    module ``run Delete should`` =
        [<Fact>]
        let ``delete file`` () =
            let uniqueTestDirectory = "test-f6c38750-9699-4dc5-80f7-118a5e987ab0"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createFile [|backupPath; "file"|]

            test <@ File.Exists (Path.Combine (backupPath, "file")) = true @>

            [
                Delete { Value = "file"; Type = Source; ContentType = File }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) []
            |> ignore

            test <@ File.Exists (Path.Combine (backupPath, "file")) = false @>

        [<Fact>]
        let ``delete directory`` () =
            let uniqueTestDirectory = "test-97cccdf0-48a6-452a-810f-883098a30e72"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createDirectory [|backupPath; "directory"|]

            test <@ Directory.Exists (Path.Combine (backupPath, "directory")) = true @>

            [
                Delete { Value = "directory"; Type = Source; ContentType = Directory }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) []
            |> ignore

            test <@ Directory.Exists (Path.Combine (backupPath, "directory")) = false @>

        [<Fact>]
        let ``delete file from alias source`` () =
            let uniqueTestDirectory = "test-d15f2f2a-873a-4149-a80b-a1b1a4e476a7"
            let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
            TestHelpers.createDirectory [|backupPath; "alias"|]
            TestHelpers.createFile [|backupPath; "alias"; "file"|]

            test <@ File.Exists (Path.Combine (backupPath, "alias", "file")) = true @>

            [
                Delete { Value = "alias\\file"; Type = Alias; ContentType = File }
            ]
            |> Process.run sourcePath backupPath (fun _ -> ()) [
                { Name = "alias"; Path = sourcePath }
            ]
            |> ignore

            test <@ File.Exists (Path.Combine (backupPath, "alias", "file")) = false @>

    [<Fact>]
    let ``should log errors without stopping sync`` () =
        let uniqueTestDirectory = "test-54ee0f2e-24e1-4f9d-9af8-1fe2d3b224d1"
        let sourcePath, backupPath = setupSyncDirectoryTest uniqueTestDirectory
        TestHelpers.createFile [|sourcePath; "file"|]

        let logs = System.Collections.Generic.List<_> ()
        [
            Add { Value = "missing file first"; Type = Source; ContentType = File }
            Add { Value = "file"; Type = Source; ContentType = File }
        ]
        |> Process.run sourcePath backupPath logs.Add []
        |> ignore

        test <@ File.Exists (Path.Combine (backupPath, "file")) = true @>
        let expectedLogs = [
            "Adding file::\"missing file first\""
            $"""Could not find file '{Path.Combine(sourcePath, "missing file first")}'."""
            "Adding file::\"file\""
        ]
        test <@ logs |> Seq.toList = expectedLogs @>
