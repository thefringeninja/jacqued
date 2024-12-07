module Jacqued.EventStorage

open System
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Microsoft.FSharp.Reflection
open SqlStreamStore
open SqlStreamStore.Streams

type WeightConverter() =
    inherit JsonConverter<Weight>()
    override this.Read(reader, _, _) = reader.GetDecimal() |> Weight
    override this.Write(writer, value, _) = writer.WriteNumberValue value.Value

type BarConverter() =
    inherit JsonConverter<Bar>()
    override this.Read(reader, _, _) = Bar.Of(reader.GetDecimal() |> Weight)

    override this.Write(writer, value, _) =
        writer.WriteNumberValue value.Weight.Value

type PlatePairConverter() =
    inherit JsonConverter<PlatePair>()

    override this.Read(reader, _, _) =
        reader.GetDecimal() |> Weight |> PlatePair

    override this.Write(writer, value, _) =
        writer.WriteNumberValue value.WeightOfEach.Value

let options =
    JsonFSharpOptions()
        .WithUnionAdjacentTag()
        .WithUnionNamedFields()
        .WithUnionUnwrapRecordCases()
        .WithUnwrapOption()
        .WithUnionUnwrapSingleCaseUnions()
        .WithUnionAllowUnorderedTag()
        .WithUnionUnwrapFieldlessTags()
        .ToJsonSerializerOptions()

options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
options.Converters.Add(WeightConverter())
options.Converters.Add(BarConverter())
options.Converters.Add(PlatePairConverter())

let typeToEventType (event: Event) =
    match event with
    | GymSetup e -> nameof GymSetup, (e :> obj)
    | MesocycleStarted e -> nameof MesocycleStarted, (e :> obj)
    | RepSetCompleted e -> nameof RepSetCompleted, (e :> obj)
    | WaveCompleted e -> nameof WaveCompleted, (e :> obj)
    | MesocycleFailed e -> nameof MesocycleFailed, (e :> obj)
    | MesocycleCompleted e -> nameof MesocycleCompleted, (e :> obj)

let eventTypeToType eventType =
    match eventType with
    | nameof GymSetup -> typeof<GymSetup>
    | nameof MesocycleStarted -> typeof<MesocycleStarted>
    | nameof RepSetCompleted -> typeof<RepSetCompleted>
    | nameof WaveCompleted -> typeof<WaveCompleted>
    | nameof MesocycleFailed -> typeof<MesocycleFailed>
    | nameof MesocycleCompleted -> typeof<MesocycleCompleted>
    | _ -> invalidOp "Invalid event"

let private deserializeData (eventType: string) (data: string) : Event =
    let unionCaseInfo =
        FSharpType.GetUnionCases(typeof<Event>)
        |> Seq.tryFind (fun case -> case.Name = eventType)

    let eventType = eventTypeToType eventType

    let event = JsonSerializer.Deserialize(data, eventType, options)

    let optionalEvent =
        unionCaseInfo
        |> Option.map (fun case -> FSharpValue.MakeUnion(case, [| event |]) :?> Event)

    match optionalEvent with
    | Some e -> e
    | None -> invalidOp "No event found"

let private deserialize (s: StreamMessage) : Event =
    let data = s.GetJsonData().Result
    deserializeData s.Type data

let private serialize (event: Event) =
    let eventType, e = typeToEventType event

    let data = JsonSerializer.Serialize(e, options)

    NewStreamMessage(Guid.NewGuid(), eventType, data)

let readStream (store: IReadonlyStreamStore) streamName =
    let stream = store.ReadStreamForwards streamName
    stream.ToBlockingEnumerable() |> Seq.map deserialize

let readAll (store: IReadonlyStreamStore) =
    let stream = store.ReadAllForwards()
    stream.ToBlockingEnumerable() |> Seq.map deserialize

let appendToStream (store: IStreamStore) (streamName: string) version events =
    let streamStoreVersion = if version = -1 then -3 else version

    async {
        let messages = events |> Seq.map serialize |> Seq.toArray

        let! choice =
            Async.AwaitTask(store.AppendToStream(streamName, streamStoreVersion, messages))
            |> Async.Catch

        return
            match choice with
            | Choice1Of2 result -> result |> Ok
            | Choice2Of2 err -> err |> Error
    }
    |> Async.RunSynchronously
