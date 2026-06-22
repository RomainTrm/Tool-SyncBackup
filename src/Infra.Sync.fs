module SyncBackup.Infra.Sync

open System
open System.IO
open Microsoft.FSharp.Core
open SyncBackup.Domain.Dsl
open SyncBackup.Domain.Sync

module InstructionsFile =
    let [<Literal>] private Accept = "Accept"

    let private buildFileContent sourceDirectory backupDirectory instructions =
        let fileLines = [
            "# Synchronizing from:"
            $"#     {sourceDirectory}"
            "# to:"
            $"#     {backupDirectory}"
            "# If you accept the following changes, uncomment the next line (remove #) and save:"
            $"# {Accept}"
            ""
            yield! instructions |> List.map SyncInstruction.serialize
            if instructions = []
            then "# No change, your backup is up to date."
        ]
        String.Join (Dsl.NewLine, fileLines)

    let save (sourceDirectory: RepositoryPath) (backupDirectory: RepositoryPath) (instructions: SyncInstruction list) =
        let fileContent = buildFileContent sourceDirectory backupDirectory instructions
        let filePath = Dsl.getSyncInstructionsFilePath sourceDirectory
        File.WriteAllText(filePath, fileContent)
        |> Ok

    let areInstructionsAccepted (repositoryPath: RepositoryPath) =
        let filePath = Dsl.getSyncInstructionsFilePath repositoryPath
        if not (File.Exists filePath)
        then Error "Missing instructions."
        else
            File.ReadAllLines filePath
            |> Array.tryFind (fun line -> line.Contains Accept)
            |> Option.map (fun line -> line.Trim() = Accept)
            |> Option.defaultValue false
            |> Ok

module Process =
    let private buildSourceFullPath repositoryPath aliases = function
        | { Type = Alias; Value = path } ->
            let path = path.Split(Path.DirectorySeparatorChar)
            let pathWithoutAliasName = Array.skip 1 path
            let alias = aliases |> List.find (fun alias -> alias.Name = path[0])
            Path.Combine(alias.Path, String.Join(Path.DirectorySeparatorChar, pathWithoutAliasName))
        | { Type = Source; Value = path } -> Path.Combine(repositoryPath, path)

    let private buildBackupFullPath repositoryPath = function
        | { Value = path } -> Path.Combine(repositoryPath, path)

    let run (sourceDirectory: RepositoryPath) (backupDirectory: RepositoryPath) (logger: string -> unit) (aliases: Alias list) (instructions: SyncInstruction list) =
        instructions
        |> List.iter (fun instruction ->
            try
                match instruction with
                | Add ({ ContentType = ContentType.File } as path) ->
                    logger $"Adding {RelativePath.serialize path}"
                    let source = buildSourceFullPath sourceDirectory aliases path
                    let backup = buildBackupFullPath backupDirectory path
                    File.Copy (source, backup)
                | Add ({ ContentType = ContentType.Directory } as path) ->
                    let backup = buildBackupFullPath backupDirectory path
                    Directory.CreateDirectory backup |> ignore<DirectoryInfo>
                    logger $"Adding {RelativePath.serialize path}"

                | Replace ({ ContentType = ContentType.File } as path) ->
                    logger $"Replacing {RelativePath.serialize path}"
                    let source = buildSourceFullPath sourceDirectory aliases path
                    let backup = buildBackupFullPath backupDirectory path
                    File.Copy (source, backup, true)
                | Replace { ContentType = ContentType.Directory } -> ()

                | Delete ({ ContentType = ContentType.File } as path) ->
                    logger $"Deleting {RelativePath.serialize path}"
                    let backup = buildBackupFullPath backupDirectory path
                    File.Delete backup
                | Delete ({ ContentType = ContentType.Directory } as path) ->
                    let backup = buildBackupFullPath backupDirectory path
                    Directory.Delete backup
                    logger $"Deleting {RelativePath.serialize path}"
            with e -> logger e.Message
        )
        Ok ()
