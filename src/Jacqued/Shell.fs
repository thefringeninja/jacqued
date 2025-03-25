namespace Jacqued

open System
open System.IO
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open AvaloniaDialogs.Views
open Elmish
open Jacqued.CommandHandlers
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Resources
open Jacqued.Util
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
          SetupComplete: bool
          Settings: Configuration.Settings
          SettingsPath: string option }

        static member zero =
            { Setup = Setup.State.zero
              Workout = Workout.State.zero
              Progress = Progress.State.zero
              Screen = Setup
              Dialog = None
              SetupComplete = false
              Settings = Configuration.Settings.zero
              SettingsPath = None }

    type Results =
        | State of State
        | Cmd of Result<Event list, exn>

    let update (store: IStreamStore) msg state =
        let read = EventStorage.readStream store
        let append = EventStorage.appendToStream store

        let gym = Gym.create read append
        let mesocycle = Mesocycle.create read append

        let saveSettings (settings: Configuration.Settings) =
            match state.SettingsPath with
            | Some settingsPath ->
                let fileInfo = FileInfo(settingsPath)
                use writer = fileInfo.CreateText()
                settings.save writer
            | _ -> ()

        let setupComplete =
            match msg with
            | Event e ->
                match e with
                | GymSetup _ -> true
                | _ -> state.SetupComplete
            | _ -> state.SetupComplete

        let results =
            match msg with
            | Msg.ApplicationError error ->
                [ let dialog, result = "", Cmd.none
                  yield { state with Dialog = dialog |> Some } |> Results.State ]
            | Msg.SelectedThemeChanged theme ->
                Theme.set theme

                let settings =
                    { state.Settings with
                        ThemeVariant = theme }

                saveSettings settings

                [ { state with Settings = settings } |> Results.State ]
            | Msg.ConfigurationSettingsLoaded settings ->
                Theme.set settings.ThemeVariant

                [ { state with Settings = settings } |> Results.State ]
            | _ ->
                try
                    [ let setup, result = Setup.update gym msg state.Setup
                      yield result |> Results.Cmd

                      let progress = Progress.update msg state.Progress

                      let workout, result =
                          Workout.update (fun () -> DateOnly.today) mesocycle msg state.Workout

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

        { state' with
            SetupComplete = setupComplete },
        cmd

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

    let init store (settings: Configuration.Settings) settingsPath () =
        let events =
            seq {
                yield Msg.ConfigurationSettingsLoaded settings
                yield! (EventStorage.readAll store) |> Seq.map Msg.Event
            }

        Seq.fold
            (fun (state, _) event -> update store event state)
            ({ State.zero with
                SettingsPath = settingsPath |> Some },
             Cmd.none)
            events
