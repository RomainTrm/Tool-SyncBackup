module SyncBackup.Cli.Worker

open Argu
open System

let [<Literal>] CurrentVersion = "1.3.1"

let private executeCommand<'c when 'c :> IArgParserTemplate> run (command: ParseResults<'c>) =
    command.GetAllResults ()
    |> List.tryExactlyOne
    |> Option.map run

type Commands =
    | [<CliPrefix(CliPrefix.None)>] Init of ParseResults<Init.Init>
    | [<CliPrefix(CliPrefix.None)>] Alias of ParseResults<Aliases.Alias>
    | [<CliPrefix(CliPrefix.None)>] Rules of ParseResults<Rules.Rule>
    | [<CliPrefix(CliPrefix.None)>] Scan of ParseResults<Scan.Scan>
    | [<CliPrefix(CliPrefix.None)>] Process of ParseResults<Sync.Process>
    | [<CliPrefix(CliPrefix.None)>] Replicate of ParseResults<Sync.Replicate>
    | [<CliPrefix(CliPrefix.None)>] Version of ParseResults<Nothing>
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Init _ -> "Initialize the current directory as a repository to synchronize."
            | Alias _ -> "Manage aliases (pointers to directories outside the repository's directory), only available for the source repository."
            | Rules _ -> "Manage rules for synchronization."
            | Scan _ -> "Reference all directories and files in the repository."
            | Process _ -> "Run synchronization process between two repositories."
            | Replicate _ -> "Replicate a backup repository (rules and content), rules like replace, preserve are applied."
            | Version _ -> "Display version of the software."

and Nothing = | [<Hidden>] Nothing
with interface IArgParserTemplate with member this.Usage = ""

let runCommand (parser: ArgumentParser<Commands>) (logger: string -> unit) argv =
    let currentDirectory = Environment.CurrentDirectory
    let configCommandInfra = Factory.configCommandInfra logger currentDirectory
    let configQueryInfra = Factory.configQueryInfra currentDirectory
    let contentCommandInfra = Factory.contentCommandInfra currentDirectory
    let syncCommandInfraFactory = Factory.syncCommandInfra logger currentDirectory
    let replicateBackupCommandInfraFactory = Factory.replicateBackupCommandInfra logger currentDirectory

    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    results.TryGetSubCommand()
    |> Option.bind (function
        | Init command -> command |> executeCommand (Init.runCommand configCommandInfra)
        | Alias command -> command |> executeCommand (Aliases.runCommand configCommandInfra configQueryInfra)
        | Rules command -> command |> executeCommand (Rules.runCommand configCommandInfra configQueryInfra)
        | Scan command -> command |> executeCommand (Scan.runCommand contentCommandInfra logger)
        | Process command -> command |> executeCommand (Sync.runSyncCommand syncCommandInfraFactory logger)
        | Replicate command -> command |> executeCommand (Sync.runReplicateCommand replicateBackupCommandInfraFactory logger)
        | Version _ -> Some (Ok CurrentVersion)
    )
    |> Option.defaultWith (fun () -> Ok (parser.PrintUsage()))
    |> function
        | Ok value -> logger value
        | Error error -> logger $"An error occured: {error}"
