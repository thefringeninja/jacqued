namespace Jacqued

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Threading
open AvaloniaDialogs.Views
open Elmish
open Jacqued.CommandHandlers
open Jacqued.Controls
open Jacqued.DSL
open Material.Icons
open SqlStreamStore

module Shell =

    type Screen =
        | Setup
        | Workout

    type State =
        { Setup: Setup.State
          Workout: Workout.State
          Progress: Progress.State
          Screen: Screen
          Dialog: string option
          SetupComplete: bool }

        static member zero =
            { Setup = Setup.State.zero
              Workout = Workout.State.zero
              Progress = Progress.State.zero
              Screen = Setup
              Dialog = None
              SetupComplete = false }

    type Results =
        | State of State
        | Cmd of Result<Event list, exn>

    let update (store: IStreamStore) msg state =
        let read = EventStorage.readStream store
        let append = EventStorage.appendToStream store

        let gym = Gym.create read append
        let mesocycle = Mesocycle.create read append
        
        let setupComplete =
            match msg with
            Event e ->
                match e with
                | GymSetup _ -> true
                | _ -> state.SetupComplete
            | _ -> state.SetupComplete

        let results =
            match msg with
            | Msg.ApplicationError error ->
                [ let dialog, result = "", Cmd.none
                  yield { state with Dialog = dialog |> Some } |> Results.State ]
            | _ ->
                try
                    [ let setup, result = Setup.update gym msg state.Setup
                      yield result |> Results.Cmd

                      let progress = Progress.update msg state.Progress

                      let workout, result =
                          Workout.update (fun () -> DateTime.Now) mesocycle msg state.Workout

                      yield result |> Results.Cmd

                      yield
                          { state with
                              Setup = setup
                              Progress = progress
                              Workout = workout }
                          |> Results.State ]
                with exn ->
                    [ exn |> Result.Error |> Results.Cmd ]

        let cmd =
            results
            |> Seq.map (fun result ->
                match result with
                | State _ -> Seq.empty
                | Cmd results ->
                    match results with
                    | Result.Ok events -> events |> Seq.map Msg.Event
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

        { state' with SetupComplete = setupComplete }, cmd

    let view state dispatch =
        let tabs =
            TabControl.create [
                TabControl.tabStripPlacement Dock.Bottom
                TabControl.viewItems [
                    if state.SetupComplete then
                        yield
                            TabItem.create [
                                TabItem.header (NavigationButton.create [ NavigationButton.content (MaterialIconKind.Barbell, "Workout") ])
                                TabItem.content (Workout.view state.Workout dispatch)
                            ]

                        yield
                            TabItem.create [
                                TabItem.header (NavigationButton.create [ NavigationButton.content (MaterialIconKind.Graph, "Progress") ])
                                TabItem.content (Progress.view state.Progress dispatch)
                            ]

                    yield
                        TabItem.create [
                            TabItem.header (NavigationButton.create [ NavigationButton.content (MaterialIconKind.Cog, "Setup") ])
                            TabItem.content (Setup.view state.Setup dispatch)
                        ]
                ]
            ]

        ReactiveDialogHost.create [
            ReactiveDialogHost.content (Panel.create [ Panel.margin 16; Panel.children [ tabs ] ])
        ]

    let init store () =
        let events =
            (EventStorage.readAll store) |> Seq.map Msg.Event

        Seq.fold (fun (state, _) event -> update store event state) (State.zero, Cmd.none) events
