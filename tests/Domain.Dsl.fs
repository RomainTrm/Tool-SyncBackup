module SyncBackup.Tests.Domain.Dsl

open System
open System.IO
open SyncBackup.Tests.Properties.CustomGenerators
open Xunit
open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open SyncBackup.Domain.Dsl

module ``build directory path should`` =

    [<Theory>]
    [<InlineData("C:\\", "C:")>]
    [<InlineData("C:\\someDir\\", "C:\\someDir")>]
    [<InlineData("C:\\someDir", "C:\\someDir")>]
    [<InlineData("C:\\someDir\"", "C:\\someDir")>]
    let ``cleanup directory path`` input expected =
        let result = DirectoryPath.build input
        test <@ result = expected @>

module ``SyncRules should`` =
    [<Property>]
    let ``parse any rule`` rule =
        let result = (SyncRules.getValue >> SyncRules.parse) rule
        test <@ result = Ok rule @>

module ``RelativePath contains should`` =
    let isValidPath path = (not<<String.IsNullOrWhiteSpace) path.Value

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return true is contained`` path =
        isValidPath path ==> lazy
        let child = { path with Value = Path.Combine(path.Value, "subpath") }
        let result = RelativePath.contains child path
        test <@ result @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return false is not contained`` path =
        isValidPath path ==> lazy
        let child = { path with Value = Path.Combine(path.Value, "subpath") }
        let result = RelativePath.contains path child
        test <@ not result @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return false if not start of child path`` path =
        isValidPath path ==> lazy
        let child = { path with Value = Path.Combine("root", path.Value, "subpath") }
        let result = RelativePath.contains child path
        test <@ not result @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return false if prefix of child path`` path =
        isValidPath path ==> lazy
        let child = { path with Value = Path.Combine($"{path.Value}-suffix", "subpath") }
        let result = RelativePath.contains child path
        test <@ not result @>

    [<Property(Arbitrary = [| typeof<PathStringGenerator> |])>]
    let ``return false when compare to itself`` path =
        isValidPath path ==> lazy
        let result = RelativePath.contains path path
        test <@ result = false @>
