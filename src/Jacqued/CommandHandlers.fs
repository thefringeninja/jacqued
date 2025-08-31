module Jacqued.CommandHandlers

open System

let private fold state evolve events =
    let acc (ev, s) e = ev + 1, evolve s e

    Seq.fold acc (-1, state) events

module Gym =
    open Gym

    let create read append =
        let streamName = "gym"

        let load () = fold () evolve (read streamName)

        let save expectedVersion events =
            append streamName expectedVersion events

        fun command ->
            let expectedVersion, state = load ()
            let events = handle command state

            match save expectedVersion events with
            | Ok _ -> Ok events
            | Error e -> Error e

module Exercises =
    open Exercises

    let private exercise =
        function
        | CalculateOneRepMax { Exercise = exercise } -> (exercise |> string).ToLowerInvariant()
        | SetWeightIncreases _ -> "all"
        | unknown -> invalidOp $"{unknown} was not recognized"

    let create append =
        let streamName exercise = $"exercise-{exercise}"

        let save exercise events = append (streamName exercise) -2 events

        fun command ->
            let exercise = exercise command

            let events = handle command

            match save exercise events with
            | Ok _ -> Ok events
            | Error e -> Error e

module Mesocycle =
    open Mesocycle

    let private mesocycleId =
        function
        | StartMesocycle { MesocycleId = MesocycleId id } -> id
        | CompleteRepSet { MesocycleId = MesocycleId id } -> id
        | CompleteWave { MesocycleId = MesocycleId id } -> id
        | FailRepSet { MesocycleId = MesocycleId id } -> id
        | unknown -> invalidOp $"{unknown} was not recognized"

    let create read append =
        let streamName mesocycleId = $"mesocycle-{mesocycleId:n}"

        let load mesocycleId state =
            fold state evolve (read (streamName mesocycleId))

        let save mesocycleId expectedVersion events =
            append (streamName mesocycleId) expectedVersion events

        fun command ->
            let id = mesocycleId command

            let expectedVersion, state = load id State.zero

            let state =
                match expectedVersion with
                | -1 -> None
                | _ -> state |> Some

            let events = handle command state

            match save id expectedVersion events with
            | Ok _ -> Ok events
            | Error e -> Error e

module AssistanceTemplate =
    open AssistanceTemplate

    let private assistanceTemplateId =
        function
        | DefineAssistanceTemplate { AssistanceTemplateId = AssistanceTemplateId id } -> id
        | RemoveAssistanceTemplate { AssistanceTemplateId = AssistanceTemplateId id } -> id
        | unknown -> invalidOp $"{unknown} was not recognized"

    let create readBackwards append hasAssistanceTemplate =
        let streamName (assistanceTemplateId: Guid) =
            $"assistanceTemplate-{assistanceTemplateId:n}"

        let load assistanceTemplateId state =
            fold state evolve (readBackwards (streamName assistanceTemplateId))

        let save assistanceTemplateId events =
            append (streamName assistanceTemplateId) -2 events

        let handleCommand =
            fun command ->
                let id = assistanceTemplateId command

                let _, state = load id State.zero

                let events = handle hasAssistanceTemplate command state

                match save id events with
                | Ok _ -> Ok events
                | Error e -> Error e

        let handleQuery =
            fun (assistanceTemplateId: AssistanceTemplateId) ->
                match
                    (readBackwards (streamName assistanceTemplateId.Value)
                     |> Seq.choose (fun e ->
                         match e with
                         | AssistanceTemplateDefined e ->
                             ({| AssistanceTemplateId = e.AssistanceTemplateId
                                 Name = e.Name
                                 Exercises = e.Exercises |})
                             |> Some
                         | _ -> None)
                     |> Seq.tryHead)
                with
                | None ->
                    {| AssistanceTemplateId = assistanceTemplateId
                       Name = ""
                       Exercises = Map.empty |}
                | Some atd -> atd

        (handleCommand, handleQuery)
