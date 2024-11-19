namespace Jacqued

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open AvaloniaDialogs.Views
open Elmish
open Jacqued.CommandHandlers
open Jacqued.Controls
open SqlStreamStore

module Shell =

    type Screen =
        | Setup
        | Workout

    type State =
        { Setup: Setup.State
          Workout: Workout.State
          Screen: Screen
          Dialog: string option }

        static member zero =
            { Setup = Setup.State.zero
              Workout = Workout.State.zero
              Screen = Setup
              Dialog = None }

    type Results =
        | State of State
        | Cmd of Result<Event list, exn>

    let update (store: IStreamStore) msg state =
        let read = EventStorage.readStream store
        let append = EventStorage.appendToStream store

        let gym = Gym.create read append
        let mesocycle = Mesocycle.create read append

        let results =
            match msg with
            | Msg.ApplicationError error ->
                [ let dialog, result = "", Cmd.none
                  yield { state with Dialog = dialog |> Some } |> Results.State ]
            | _ ->
                [ let setup, result = Setup.update msg state.Setup gym
                  yield result |> Results.Cmd

                  let workout, result = Workout.update msg state.Workout mesocycle
                  yield result |> Results.Cmd

                  yield
                      { state with
                          Setup = setup
                          Workout = workout }
                      |> Results.State ]

        let cmd =
            results
            |> Seq.map (fun result ->
                match result with
                | State _ -> Seq.empty
                | Cmd results ->
                    match results with
                    | Result.Ok events -> events |> List.toSeq |> Seq.map (fun event -> event |> Msg.Event)
                    | Result.Error err -> [ err.Message |> ApplicationError.Message |> Msg.ApplicationError ]) // TODO make dialog message
            |> Seq.concat
            |> Seq.map (fun msg -> msg |> Cmd.ofMsg)
            |> Cmd.batch

        let state' =
            match (results |> List.tryLast) with
            | None -> state
            | Some item ->
                match item with
                | Results.Cmd _ -> state
                | Results.State state -> state

        state', cmd

    let view state dispatch =
        let view =
            (match state.Workout.Gym with
             | Some _ -> Workout.view state.Workout dispatch |> generalize
             | None -> Setup.view state.Setup dispatch)

        ReactiveDialogHost.create [ ReactiveDialogHost.content (Panel.create [ Panel.margin 16; Panel.children [ view ] ]) ]

    let init store () =
        let events =
            (EventStorage.readAll store) |> Seq.map (fun event -> event |> Msg.Event)

        Seq.fold (fun (state, _) event -> update store event state) (State.zero, Cmd.none) events
