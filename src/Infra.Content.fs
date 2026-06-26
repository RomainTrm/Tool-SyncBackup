module SyncBackup.Infra.Content

open System
open System.IO
open Microsoft.FSharp.Core
open SyncBackup
open SyncBackup.Infra
open SyncBackup.Domain.Dsl

module Scan =
    let private excludeConfigFolder (directoryPath: string) =
        directoryPath.Contains Dsl.ConfigDirectory |> not

    let private sourceRelativePath (repositoryPath: RepositoryPath) fullPath (contentType: ContentType) =
        let relativePath = Path.GetRelativePath(repositoryPath, fullPath)
        { Value = relativePath; ContentType = contentType; Type = Source }

    let private aliasRelativePath (alias: Alias) fullPath (contentType: ContentType) =
        let relativePath = Path.GetRelativePath(alias.Path, fullPath)
        { Value = Path.Combine(alias.Name, relativePath); ContentType = contentType; Type = Alias }

    let private setPrecision (dateTime: DateTime) =
        DateTime(dateTime.Year, dateTime.Month, dateTime.Day, dateTime.Hour, dateTime.Minute, dateTime.Second)

    let rec private scan' (currentDirectoryPath: DirectoryPath) (buildRelativePath: string -> ContentType -> RelativePath) =
        let files =
            Directory.GetFiles currentDirectoryPath
            |> Seq.map (fun fullFilePath ->
                let path = buildRelativePath fullFilePath ContentType.File
                let lastWriteTime = File.GetLastWriteTimeUtc fullFilePath
                { Path = path; LastWriteTime = Some (setPrecision lastWriteTime) }
            )
            |> Seq.toList

        let directories =
            Directory.GetDirectories currentDirectoryPath
            |> Seq.filter excludeConfigFolder
            |> Seq.fold (fun acc directoryPath ->
                let path = buildRelativePath directoryPath ContentType.Directory
                let children =
                    try
                        scan' directoryPath buildRelativePath
                    with // Windows may detect a directory but fail to access it because it's not there
                    | :? UnauthorizedAccessException -> []
                List.concat [ acc; [{ Path = path; LastWriteTime = None }]; children ]
            ) []

        List.concat [ files; directories ]

    let run (repositoryPath: RepositoryPath) (aliases: Alias list) =
        let sourceDirectoryContent = scan' repositoryPath (sourceRelativePath repositoryPath)
        let aliasesDirectoriesContent =
            List.collect (fun (alias: Alias) ->
                let aliasContent = scan' alias.Path (aliasRelativePath alias)
                let aliasRoot = {
                    Path = { Value = alias.Name; Type = Alias; ContentType = Directory }
                    LastWriteTime = None
                }
                aliasRoot::aliasContent
            ) aliases
        List.concat [ sourceDirectoryContent; aliasesDirectoriesContent ]

module ScanFile =
    let rec private print scanResult =
        $"{ScanDiff.activeLine scanResult.Diff}{SyncRules.getValue scanResult.SyncRule} {ScanDiff.serialize scanResult.Diff} {RelativePath.serialize scanResult.Path}"

    let private buildFileContent repositoryType (scanResults: ScanResult list) =
        let fileLines = [
            "# Repository scan complete!"
            "# Use '#' to comment a line"
            $"# You can specify rules to every line (directories and files), by default '{SyncRules.getValue NoRule}' is set"
            "# Available rules as follows:"
            yield! SyncRules.getRulesAvailable repositoryType
                    |> Seq.map (SyncRules.getDescription >> sprintf "# - %s")
            ""
            yield! scanResults |> List.map print
            if scanResults = []
            then "# No change, your repository is up to date."
        ]
        String.Join (Dsl.NewLine, fileLines)

    let writeFile (repositoryPath: RepositoryPath) (repositoryType: RepositoryType) (scanResults: ScanResult list) =
        let fileContent = buildFileContent repositoryType scanResults
        let filePath = Dsl.getScanFileFilePath repositoryPath
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let private removeComments (contentLine: string) =
        not (String.IsNullOrWhiteSpace contentLine || contentLine.TrimStart().StartsWith "#")

    let private parseSyncResult (contentLine: string) =
        match contentLine.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.toList with
        | rule::scanDiff::path ->
            result {
                let! rule = SyncRules.parse rule
                let! path = String.Join(' ', path) |> RelativePath.deserialize
                let! diff = ScanDiff.deserialize scanDiff
                return { SyncRule = rule; Path = path; Diff = diff }
            }
        | _ -> Error "Invalid format"

    let readFile (repositoryPath: RepositoryPath) =
        Dsl.getScanFileFilePath repositoryPath
        |> File.ReadAllLines
        |> Seq.filter removeComments
        |> Seq.map parseSyncResult
        |> Seq.fold (fun result content ->
            match result, content with
            | Ok result, Ok content -> Ok (content::result)
            | _, Error error
            | Error error, _ -> Error error
        ) (Ok [])
        |> Result.map List.rev

module TrackFile =
    let save (repositoryPath: RepositoryPath) (contents: Content list) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        let contentLines = contents |> List.map Content.serialize
        File.WriteAllLines (filePath, contentLines)
        Ok ()

    let load (repositoryPath: RepositoryPath) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        if not (File.Exists filePath)
        then Ok []
        else
            File.ReadAllLines filePath
            |> Seq.fold (fun paths line ->
                paths
                |> Result.bind (fun contents ->
                    Content.deserialize line
                    |> Result.map (fun content -> content::contents)
                )
            ) (Ok [])
            |> Result.map List.rev

    let reset (repositoryPath: RepositoryPath) =
        let filePath = Dsl.getTrackFileFilePath repositoryPath
        if (File.Exists filePath)
        then File.Delete filePath
        Ok ()
