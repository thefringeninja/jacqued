module Jacqued.Data

open System
open System.Globalization
open System.IO
open Avalonia.Platform.Storage
open CsvHelper
open CsvHelper.Configuration
open FSharp.Control
open Jacqued.Util
open SqlStreamStore
open SqlStreamStore.Streams

let private csvConfiguration =
    CsvConfiguration(CultureInfo.InvariantCulture, HasHeaderRecord = false)

type private EventRecord =
    { streamId: string
      streamVersion: int32
      messageId: Guid
      ``type``: string
      data: string }

type private IStreamStore with
    member this.clear() =
        task {
            let! streams = this.ListStreams(Int32.MaxValue)

            do!
                streams.StreamIds
                |> TaskSeq.ofArray
                |> TaskSeq.iterAsync (this.DeleteStream >> Task.ofTask)
        }

type IBackupManager =
    abstract member backup: unit -> Async<unit>
    abstract member restore: unit -> Async<unit>

type BackupManager(store: IStreamStore, storageProvider: IStorageProvider, folder: DirectoryInfo, now: unit -> DateTime) =
    interface IBackupManager with
        member this.backup() =
            task {
                
                use! folder = storageProvider.TryGetFolderFromPathAsync(folder.FullName)

                let filename = $"backup_{now ():yyyyMMddThhmmss}.bak"

                use! file = folder.CreateFileAsync(filename)
                use! stream = file.OpenWriteAsync()
                use writer = new CsvWriter(new StreamWriter(stream), csvConfiguration)

                let header = Unchecked.defaultof<EventRecord>

                writer.WriteField(nameof header.streamId)
                writer.WriteField(nameof header.streamVersion)
                writer.WriteField(nameof header.messageId)
                writer.WriteField(nameof header.``type``)
                writer.WriteField(nameof header.data)
                do! writer.NextRecordAsync()

                let writeRecordAsync (record: EventRecord) =
                    task {
                        writer.WriteField(record.streamId)
                        writer.WriteField<int32>(record.streamVersion)
                        writer.WriteField<Guid>(record.messageId)
                        writer.WriteField(record.``type``)
                        writer.WriteField(record.data)
                        do! writer.NextRecordAsync()
                    }

                do!
                    store.ReadAllForwards()
                    |> TaskSeq.mapAsync (fun message ->
                        task {
                            let! data = message.GetJsonData()

                            return
                                { messageId = message.MessageId
                                  streamId = message.StreamId
                                  streamVersion = message.StreamVersion
                                  ``type`` = message.Type
                                  data = data }

                        })
                    |> TaskSeq.iterAsync writeRecordAsync
            }
            |> Async.AwaitTask

        member this.restore() =
            task {
                let! folder = storageProvider.TryGetFolderFromPathAsync(folder.FullName)

                let! files =
                    storageProvider.OpenFilePickerAsync(FilePickerOpenOptions(AllowMultiple = false, SuggestedStartLocation = folder))

                match files |> Seq.tryHead with
                | Some file ->
                    use file = file

                    use! stream = file.OpenReadAsync()
                    use reader = new CsvReader(new StreamReader(stream), csvConfiguration)

                    let! _ = reader.ReadAsync()

                    do! store.clear ()

                    do!
                        taskSeq {
                            while! reader.ReadAsync() do
                                yield
                                    { streamId = reader.Item 0
                                      streamVersion = ((reader.Item 1) |> Int32.Parse) - 1
                                      messageId = (reader.Item 2) |> Guid.Parse
                                      ``type`` = reader.Item 3
                                      data = reader.Item 4 }
                        }
                        |> TaskSeq.iterAsync (fun message ->
                            task {
                                let expectedVersion =
                                    if message.streamVersion = -1 then
                                        -3
                                    else
                                        message.streamVersion

                                let! appendResult =
                                    store.AppendToStream(
                                        message.streamId,
                                        expectedVersion,
                                        NewStreamMessage(message.messageId, message.``type``, message.data)
                                    )

                                ()
                            })
                | _ -> ()
            }
            |> Async.AwaitTask
