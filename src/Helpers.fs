module [<AutoOpen>] SyncBackup.Helpers

type ResultBuilder() =
    member x.Bind(comp, func) = Result.bind func comp
    member x.Return(value) = Ok value
    member x.ReturnFrom(value) = value

let result = ResultBuilder ()

let fold folder state values =
    values
    |> Seq.fold (fun acc value ->
        acc
        |> Result.bind (fun acc -> folder acc value)
    ) (Ok state)

let foldResults<'a, 'b> : Result<'a list, 'b> list -> Result<'a list, 'b> =
    fold (fun acc items ->
        result {
            let! items = items
            return List.concat [acc; items]
        }
    ) []
