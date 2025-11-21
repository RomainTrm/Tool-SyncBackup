module SyncBackup.Tests.Commands.Scan

open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Domain
open SyncBackup.Commands.Scan
open SyncBackup.Tests.Properties.CustomGenerators

let defaultInfra = {
    LoadConfig = fun _ -> failwith "not implemented"
    LoadTrackFile = fun _ -> failwith "not implemented"
    ScanRepositoryContent = fun _ -> failwith "not implemented"
    SaveScanFileContent = fun _ -> failwith "not implemented"
    OpenScanFileForUserEdition = fun _ -> failwith "not implemented"
    ReadScanFileContent = fun _ -> failwith "not implemented"
    SaveTrackFile = fun _ -> failwith "not implemented"
    SaveRules = fun _ -> failwith "not implemented"
    ResetScan = fun _ -> failwith "not implemented"
}

let defaultConfig : Dsl.RepositoryConfig = {
    Version = Dsl.RepositoryConfigVersion
    Type = Dsl.RepositoryType.Source
    Aliases = []
    Rules = []
}

module ``scanRepositoryContent should`` =
    [<Property(Arbitrary = [| typeof<NonWhiteSpaceStringGenerator>; typeof<SourceRepositoryRulesOnlyGenerator> |])>]
    let ``retrieve content for repository, save it then open editor, then save track file and rules`` aliases (content: (Dsl.RelativePath * Dsl.Content * Dsl.ScanResult) list) =
        content <> [] ==> lazy
        let contentEdited =
            content
            |> List.map (fun (path, _, scanResult) -> { scanResult with Path = path })
            |> List.distinctBy _.Path
        let content =
            content
            |> List.map (fun (path, content, _) -> { content with Path = path })

        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok { defaultConfig with Aliases = aliases }
                LoadTrackFile = fun () -> Ok []
                ScanRepositoryContent = fun a ->
                    test <@ a = aliases @>
                    content
                SaveScanFileContent = fun repositoryType _ ->
                    test <@ repositoryType = defaultConfig.Type @>
                    calls.Add "save temp file" |> Ok
                OpenScanFileForUserEdition = fun () -> calls.Add "open editor" |> Ok
                ReadScanFileContent = fun () -> Ok contentEdited
                SaveTrackFile = fun c ->
                    let expected = contentEdited |> List.filter (fun scan -> scan.Diff = Dsl.AddedToRepository) |> List.map _.Path
                    let c = c |> List.map _.Path
                    test <@ Set c = Set expected @>
                    calls.Add "save track file" |> Ok
                SaveRules = fun rules ->
                    let expected = contentEdited |> List.filter (fun scanResult -> scanResult.SyncRule <> Dsl.NoRule) |> List.map _.Rule
                    test <@ rules = expected @>
                    calls.Add "save rules" |> Ok
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Ok () @>
        test <@ calls |> Seq.toList = ["save temp file"; "open editor"; "save track file"; "save rules"] @>

    [<Fact>]
    let ``build apply existing rules to scanned content`` () =
        let savedRules = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok {
                    defaultConfig with
                        Rules = [
                            { Path = { Type = Dsl.PathType.Source; Value = "path1"; ContentType = Dsl.Directory }; SyncRule = Dsl.Include }
                            { Path = { Type = Dsl.PathType.Source; Value = "path2"; ContentType = Dsl.Directory }; SyncRule = Dsl.Exclude }
                        ]
                }
                LoadTrackFile = fun () -> Ok []
                ScanRepositoryContent = fun _ -> [
                    { Path = { Type = Dsl.PathType.Source; Value = "path1"; ContentType = Dsl.Directory }; LastWriteTime = None }
                    { Path = { Type = Dsl.PathType.Source; Value = "path2"; ContentType = Dsl.Directory }; LastWriteTime = None }
                    { Path = { Type = Dsl.PathType.Source; Value = "path3"; ContentType = Dsl.Directory }; LastWriteTime = None }
                ]
                SaveScanFileContent = fun repositoryType rules ->
                    test <@ repositoryType = defaultConfig.Type @>
                    savedRules.AddRange rules
                    Error "I don't want to setup the rest of the infra"
        }

        let _ = scanRepositoryContent infra ()

        let expected: Dsl.ScanResult list = [
            { Path = { Type = Dsl.PathType.Source; Value = "path1"; ContentType = Dsl.Directory }; SyncRule = Dsl.Include; Diff = Dsl.AddedToRepository }
            { Path = { Type = Dsl.PathType.Source; Value = "path2"; ContentType = Dsl.Directory }; SyncRule = Dsl.Exclude; Diff = Dsl.AddedToRepository }
            { Path = { Type = Dsl.PathType.Source; Value = "path3"; ContentType = Dsl.Directory }; SyncRule = Dsl.NoRule; Diff = Dsl.AddedToRepository }
        ]
        test <@ savedRules |> Seq.toList = expected @>

    [<Property>]
    let ``return nothing when empty repository`` aliases =
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok { defaultConfig with Aliases = aliases }
                LoadTrackFile = fun () -> Ok []
                ScanRepositoryContent = fun a ->
                    test <@ a = aliases @>
                    []
                SaveScanFileContent = fun _ result ->
                    test <@ result = [] @>
                    Ok ()
                OpenScanFileForUserEdition = fun () -> Ok ()
                ReadScanFileContent = fun () -> Ok []
                SaveTrackFile = fun _ -> Ok ()
                SaveRules = fun _ -> Ok ()
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Ok () @>

    [<Fact>]
    let ``remove deleted elements from track and not discard unmodified elements`` () =
        let savedTrackedElements = System.Collections.Generic.List<_> ()
        let scanResult = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok defaultConfig
                LoadTrackFile = fun () -> Ok [
                    { Path = { Type = Dsl.PathType.Source; Value = "path1"; ContentType = Dsl.Directory }; LastWriteTime = None }
                    { Path = { Type = Dsl.PathType.Source; Value = "path2"; ContentType = Dsl.Directory }; LastWriteTime = None }
                    { Path = { Type = Dsl.PathType.Source; Value = "path3"; ContentType = Dsl.Directory }; LastWriteTime = None }
                ]
                ScanRepositoryContent = fun _ -> [
                    { Path = { Type = Dsl.PathType.Source; Value = "path2"; ContentType = Dsl.Directory }; LastWriteTime = None }
                    { Path = { Type = Dsl.PathType.Source; Value = "path3"; ContentType = Dsl.Directory }; LastWriteTime = None }
                    { Path = { Type = Dsl.PathType.Source; Value = "path4"; ContentType = Dsl.Directory }; LastWriteTime = None }
                ]
                SaveScanFileContent = fun _ -> scanResult.AddRange >> Ok
                OpenScanFileForUserEdition = Ok
                ReadScanFileContent = fun () -> scanResult |> Seq.toList |> Ok
                SaveTrackFile = savedTrackedElements.AddRange >> Ok
                SaveRules = fun _ -> Ok ()
        }

        let _ = scanRepositoryContent infra ()

        let expected : Dsl.Content list = [
            { Path = { Type = Dsl.PathType.Source; Value = "path2"; ContentType = Dsl.Directory }; LastWriteTime = None }
            { Path = { Type = Dsl.PathType.Source; Value = "path3"; ContentType = Dsl.Directory }; LastWriteTime = None }
            { Path = { Type = Dsl.PathType.Source; Value = "path4"; ContentType = Dsl.Directory }; LastWriteTime = None }
        ]
        test <@ savedTrackedElements |> Seq.toList = expected @>

    [<Fact>]
    let ``return error if user set an invalid rule for the repository`` () =
        let invalidRule = Dsl.SyncRules.AlwaysReplace
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok {
                    defaultConfig with
                        Type = Dsl.RepositoryType.Source
                }
                LoadTrackFile = fun () -> Ok []
                ScanRepositoryContent = fun _ -> [
                    { Path = { Type = Dsl.PathType.Source; Value = "path1"; ContentType = Dsl.Directory }; LastWriteTime = None }
                ]
                SaveScanFileContent = fun repositoryType _ ->
                    test <@ repositoryType = defaultConfig.Type @>
                    Ok ()
                OpenScanFileForUserEdition = Ok
                ReadScanFileContent = fun () -> Ok [
                    { SyncRule = invalidRule; Path = { Type = Dsl.PathType.Source; Value = "path1"; ContentType = Dsl.Directory }; Diff = Dsl.ScanDiff.AddedToRepository }
                ]
        }

        let result = scanRepositoryContent infra ()
        test <@ result = Error $"The rule \"{Dsl.SyncRules.getValue invalidRule}\" can't be applied to this repository type." @>

module ``reset repository should`` =
    [<Fact>]
    let ``return error if not in a repository`` () =
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Error "Not in a repository"
                ResetScan = calls.Add >> Ok
        }

        let result = reset infra ()

        test <@ result = Error "Not in a repository" @>
        test <@ Seq.isEmpty calls @>

    [<Fact>]
    let ``reset repository`` () =
        let calls = System.Collections.Generic.List<_> ()
        let infra = {
            defaultInfra with
                LoadConfig = fun () -> Ok defaultConfig
                ResetScan = calls.Add >> Ok
        }

        let result = reset infra ()

        test <@ result = Ok () @>
        test <@ (not<<Seq.isEmpty) calls @>
