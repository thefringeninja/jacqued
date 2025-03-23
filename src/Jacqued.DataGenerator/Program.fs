open System
open System.IO
open FSharp.Control
open FSharp.SystemCommandLine
open Jacqued
open Jacqued.EventStorage
open Microsoft.Data.Sqlite
open SqlStreamStore

let handler (count, output:FileInfo) =
    let cs = SqliteConnectionStringBuilder()
    cs.DataSource <- output.FullName

    let settings = SqliteStreamStoreSettings(cs.ToString())
    settings.GetUtcNow <- (fun () -> DateTime.UtcNow)
    use streamStore = new SqliteStreamStore(settings)
    streamStore.CreateSchemaIfNotExists()

    let events = Generator.generate count

    let append d =
        let streamName = d |> fst
        let e = [ d |> snd ]
        appendToStream streamStore streamName -2 e |> ignore

    events |> Seq.iter append

    // streamStore.ReadAllForwards()
    // |> TaskSeq.iterAsync (fun message ->
    //     task {
    //         let! data = message.GetJsonData()
    //         printfn $"""{{ "streamName": "{message.StreamId}", "type": "{message.Type}", "data": {data} }}"""
    //     })
    // |> ignore
    0

[<EntryPoint>]
let main argv =
    rootCommand argv {
        inputs (
            Input.Option<int>(
                aliases = [ "--count" ],
                description = "The number of mesocycles to complete for each exercise",
                defaultValue = 1
            ),
            Input.Option<FileInfo>(
                aliases = [ "--output" ],
                description = "The path to the output database",
                defaultValue = FileInfo("jacqued.db")
            )
        )

        setHandler handler
    }
