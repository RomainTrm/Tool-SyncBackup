module SyncBackup.Tests.Properties.CustomGenerators

open System
open FsCheck
open SyncBackup.Domain

type NonWhiteSpaceStringGenerator () =
    static member String() = {
        new Arbitrary<String>() with
            override x.Generator =
                Arb.Default.NonWhiteSpaceString ()
                |> Arb.toGen
                |> Gen.map _.Get
        }

let invalidPathChars = [
    yield! System.IO.Path.GetInvalidPathChars()
    yield! System.IO.Path.GetInvalidFileNameChars()
]

type PathStringGenerator () =
    static member String() = {
        new Arbitrary<String>() with
            override x.Generator =
                Arb.Default.NonWhiteSpaceString ()
                |> Arb.toGen
                |> Gen.map _.Get
                |> Gen.map (FSharp.Core.String.filter (fun c -> not (List.contains c invalidPathChars)))
                |> Gen.filter (not << String.IsNullOrWhiteSpace)
        }

type SourceRepositoryRulesOnlyGenerator () =
    static member String() = {
        new Arbitrary<Dsl.SyncRules>() with
            override x.Generator =
                Dsl.RepositoryType.Source
                |> Dsl.SyncRules.getRulesAvailable
                |> Gen.elements
        }
