module SyncBackup.Domain.Dsl

open System
open SyncBackup

let [<Literal>] RepositoryConfigVersion = "1.0"
let [<Literal>] PathLevelSeparator = '\\'

type DirectoryPath = string
module DirectoryPath =
    let build (value: DirectoryPath) = value.Replace('/', PathLevelSeparator).TrimEnd [| PathLevelSeparator; '\"' |]

type FilePath = string
/// Some path provided by the user, code doesn't know if it points to an element in the source or in an alias, neither if it's a directory or a file
type UnverifiedPath = string
/// Root path of the repository
type RepositoryPath = DirectoryPath

type RepositoryType = Source | Backup
type RepositoryConfig = {
    Version: string
    Type: RepositoryType
    Aliases: Alias list
    Rules: Rule list
}
and Alias = {
    Name: string
    Path: DirectoryPath
}
and Rule = {
    Path: RelativePath
    SyncRule: SyncRules
}
and RelativePath = {
    Value: RelativePathValue
    Type: PathType
    ContentType: ContentType
}
and RelativePathValue = string
and PathType = Source | Alias
and ContentType = Directory | File
and SyncRules =
    | NoRule
    | Exclude
    | Include
    | AlwaysReplace
    | NotSave
    | NotDelete
and ScanDiff =
    | AddedToRepository
    | RemovedFromRepository
    | RuleReminder
    | Updated
and ScanResult = {
    Path: RelativePath
    SyncRule: SyncRules
    Diff: ScanDiff
} with member this.Rule = { Path = this.Path; SyncRule = this.SyncRule }
and Content = {
    Path: RelativePath
    LastWriteTime: DateTime option
}

module RelativePath =
    let [<Literal>] AliasSymbol = "*"
    let markAlias = function
        | { Type = Alias } -> AliasSymbol
        | { Type = Source } -> ""

    let [<Literal>] FilePrefix = "file::"
    let [<Literal>] DirectoryPrefix = "dir::"

    let printContentType = function
        | { ContentType = File } -> FilePrefix
        | { ContentType = Directory } -> DirectoryPrefix

    let serialize path = $"{printContentType path}\"{markAlias path}{path.Value}\""

    let deserialize (strValue: string) =
        let buildPath (path: string) =
            path.Replace("\"", "")
                .Replace(AliasSymbol, "")
                .Replace(FilePrefix, "")
                .Replace(DirectoryPrefix, "")

        match strValue with
        | _ when strValue.StartsWith $"{FilePrefix}\"{AliasSymbol}" ->
            Ok { Value = buildPath strValue; ContentType = ContentType.File; Type = Alias }
        | _ when strValue.StartsWith $"{DirectoryPrefix}\"{AliasSymbol}" ->
            Ok { Value = buildPath strValue; ContentType = ContentType.Directory; Type = Alias }
        | _ when strValue.StartsWith FilePrefix ->
            Ok { Value = buildPath strValue; ContentType = ContentType.File; Type = Source }
        | _ when strValue.StartsWith DirectoryPrefix ->
            Ok { Value = buildPath strValue; ContentType = ContentType.Directory; Type = Source }
        | _ -> Error "Invalid format"

    let contains child parent =
        match child, parent with
        | _ when child = parent -> false
        | { Value = childPath }, { Value = parentPath } -> childPath.StartsWith $"{parentPath}{PathLevelSeparator}"

    let getParents child =
        child.Value.Split PathLevelSeparator
        |> Seq.fold (fun paths name ->
            paths
            |> List.tryHead
            |> Option.map (fun lastPath -> $"{lastPath}{PathLevelSeparator}{name}")
            |> Option.map (fun newPath -> newPath::paths)
            |> Option.defaultValue [name]
        ) []
        |> Seq.skip 1
        |> Seq.map (fun path -> { Value = path; ContentType = ContentType.Directory; Type = child.Type })
        |> Seq.rev
        |> Seq.toList

module SyncRules =
    let getValue = function
        | NoRule -> "norule"
        | Exclude -> "exclude"
        | Include -> "include"
        | AlwaysReplace -> "replace"
        | NotSave -> "ignore"
        | NotDelete -> "preserve"

    let getDescription = function
        | NoRule -> $"{getValue NoRule} (default): no specific rule specified for this directory or file. Rules will be inherited, if no rule specified, then it will be included to backup."
        | Exclude -> $"{getValue Exclude}: this directory or file must not be saved into backup, even if parents are included."
        | Include -> $"{getValue Include}: this directory or file must be saved into backup, even if parents are excluded."
        | AlwaysReplace -> $"{getValue AlwaysReplace}: this directory or file is always be replaced, even if it already exists in backup."
        | NotSave -> $"{getValue NotSave}: this directory or file must not be saved into backup, even if present in the source repository."
        | NotDelete -> $"{getValue NotDelete}: this directory or file must not be deleted from backup, even if the source repository does not include it."

    let getRulesAvailable = function
        | RepositoryType.Source -> [NoRule; Exclude; Include]
        | RepositoryType.Backup -> [NoRule; AlwaysReplace; NotSave; NotDelete]

    let parse = function
        | "norule" -> Ok NoRule
        | "exclude" -> Ok Exclude
        | "include" -> Ok Include
        | "replace" -> Ok AlwaysReplace
        | "ignore" -> Ok NotSave
        | "preserve" -> Ok NotDelete
        | _ -> Error "Invalid rule"

module ScanResult =
    let build diff (rule: Rule) = {
        Path = rule.Path
        SyncRule = rule.SyncRule
        Diff = diff
    }

module ScanDiff =
    let serialize = function
        | AddedToRepository -> "(added)"
        | RemovedFromRepository -> "(removed)"
        | RuleReminder -> "(nochange)"
        | Updated -> "(updated)"

    let deserialize = function
        | "(added)" -> Ok AddedToRepository
        | "(removed)" -> Ok RemovedFromRepository
        | "(nochange)" -> Ok RuleReminder
        | "(updated)" -> Ok Updated
        | _ -> Error "Invalid diff"

    let activeLine = function
        | AddedToRepository -> ""
        | RemovedFromRepository -> ""
        | RuleReminder -> "# "
        | Updated -> ""

module Content =
    let serialize (content: Content) =
        let path = RelativePath.serialize content.Path
        let lastWriteTime =
            content.LastWriteTime
            |> Option.map (fun dt -> $"::{dt:``yyyy-MM-dd-HH-mm-ss``}")
            |> Option.defaultValue ""
        $"{path}{lastWriteTime}"

    let deserialize (contentStr: string) =
        match contentStr.Split("::") with
        | [| _; _ |] ->
            RelativePath.deserialize contentStr
            |> Result.map (fun path -> { Path = path; LastWriteTime = None })
        | [| contentType; contentPath; lastWriteTime |] ->
            result {
                let! relativePath = RelativePath.deserialize $"{contentType}::{contentPath}"
                let lastWriteTime =  DateTime.ParseExact(lastWriteTime, "yyyy-MM-dd-HH-mm-ss", System.Globalization.CultureInfo.InvariantCulture)
                return { Path = relativePath; LastWriteTime = Some lastWriteTime }
            }
        | _ ->  Error "Invalid format"
