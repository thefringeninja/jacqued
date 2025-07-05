module Jacqued.CommandHandlers

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

module Mesocycle =
    open Mesocycle

    let mesocycleId =
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
