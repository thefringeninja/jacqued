namespace Jacqued

open System
open System.Threading.Tasks
open Avalonia.Controls
open Avalonia.Data
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Threading
open AvaloniaDialogs.Views
open Elmish
open Jacqued.CommandHandlers
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Data
open Jacqued.Design
open Jacqued.Msg
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
          SetupComplete: bool
          AsyncOperationInProgress: bool
          Settings: Jacqued.Settings }

        static member zero =
            { Setup = Setup.State.zero
              Workout = Workout.State.zero
              Progress = Progress.State.zero
              Screen = Setup
              SetupComplete = false
              AsyncOperationInProgress = false
              Settings = Settings.zero }

    type private Update =
        | State of State
        | Events of Result<Event list, exn>
        | Cmd of Cmd<Msg>

    let private exnToError (ex: Exception) =
        (match ex with
         | :? AggregateException as ex -> ex.Flatten().Message
         | _ -> ex.Message)
        |> Result.Error

    let rec update (store: IStreamStore) (backupManager: IBackupManager) msg state =
        let read = EventStorage.readStream store
        let append = EventStorage.appendToStream store

        let gym = Gym.create read append

        let oneRepMaxes = OneRepMaxes.create append
        let mesocycle = Mesocycle.create read append

        let workout command =
            match command with
            | CalculateOneRepMax _ -> oneRepMaxes command
            | other -> mesocycle other

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
                let message =
                    match error with
                    | Exception ex -> ex.Message
                    | Message message -> message

                do
                    Dispatcher.UIThread.InvokeAsync<Optional<EventArgs>>(fun _ ->
                        SingleActionDialog(Message = message, ButtonText = "OK").ShowAsync())
                    |> Async.AwaitTask
                    |> Async.Ignore
                    |> ignore

                []
            | Msg.Data msg ->
                match msg with
                | Data.BeginBackup ->
                    [ Cmd.OfAsync.either
                          backupManager.backup
                          ()
                          (Ok >> Data.CompleteBackup >> Msg.Data)
                          (exnToError >> Data.CompleteBackup >> Msg.Data)
                      |> Update.Cmd
                      { state with
                          AsyncOperationInProgress = true }
                      |> Update.State ]
                | Data.CompleteBackup msg ->
                    [ match msg with
                      | Ok _ -> ()
                      | Error error -> yield error |> Message |> Msg.ApplicationError |> Cmd.ofMsg |> Update.Cmd
                      yield
                          { state with
                              AsyncOperationInProgress = false }
                          |> Update.State ]
                | Data.BeginRestore ->
                    [ Cmd.OfAsync.either
                          backupManager.restore
                          ()
                          (Ok >> Data.CompleteRestore >> Msg.Data)
                          (exnToError >> Data.CompleteRestore >> Msg.Data)
                      |> Update.Cmd
                      { state with
                          AsyncOperationInProgress = true }
                      |> Update.State ]
                | Data.CompleteRestore msg ->
                    let state', cmd =
                        match msg with
                        | Ok _ ->
                            Seq.fold
                                (fun state event -> (update store backupManager event state) |> fst)
                                { State.zero with
                                    State.Settings = state.Settings }
                                ((EventStorage.readAll store) |> Seq.map Msg.Event),
                            Cmd.none
                        | Error error -> state, error |> Message |> Msg.ApplicationError |> Cmd.ofMsg

                    [ yield cmd |> Update.Cmd
                      yield
                          { state' with
                              AsyncOperationInProgress = false }
                          |> Update.State ]
            | _ ->
                try
                    [ let setup, result = Setup.update gym msg state.Setup
                      yield result |> Update.Events

                      let progress = Progress.update msg state.Progress

                      let workout, result =
                          Workout.update (fun () -> DateOnly.today) workout msg state.Workout

                      yield result |> Update.Events

                      let settings, result = Configuration.update msg state.Settings

                      yield
                          { state with
                              Setup = setup
                              Progress = progress
                              Workout = workout
                              Settings = settings }
                          |> Update.State ]
                with exn ->
                    [ exn |> Result.Error |> Update.Events ]

        let cmd =
            results
            |> Seq.map (fun result ->
                match result with
                | State _ -> Seq.empty
                | Cmd cmd -> seq { cmd }
                | Events results ->
                    (match results with
                     | Result.Ok events -> events |> Seq.map Msg.Event
                     | Result.Error err -> [ err.Message |> ApplicationError.Message |> Msg.ApplicationError ])
                    |> Seq.map (fun msg -> msg |> Cmd.ofMsg))
            |> Seq.concat
            |> Cmd.batch

        let state' =
            match (results |> List.tryLast) with
            | None -> state
            | Some item ->
                match item with
                | Update.State state -> state
                | _ -> state

        { state' with
            SetupComplete = setupComplete },
        cmd

    let view state dispatch =
        let backupClick _ =
            Data.BeginBackup |> Msg.Data |> dispatch

        let restoreClick _ =
            Data.BeginRestore |> Msg.Data |> dispatch

        let appBar =
            TopAppBar.create [
                TopAppBar.title "Jacqued"
                TopAppBar.trailing [
                    FlatButton.create [
                        DockPanel.dock Dock.Right
                        FlatButton.content MaterialIconKind.BackupRestore
                        FlatButton.onClick backupClick
                    ]
                    FlatButton.create [
                        DockPanel.dock Dock.Right
                        FlatButton.content MaterialIconKind.Restore
                        FlatButton.onClick restoreClick
                    ]
                ]
                DockPanel.dock Dock.Top
            ]

        let progress =
            if state.AsyncOperationInProgress then
                ProgressBar.create [ ProgressBar.isIndeterminate true; DockPanel.dock Dock.Top ]
                |> generalize
            else
                StackPanel.create [ StackPanel.margin (0, 4, 0, 0); DockPanel.dock Dock.Top ]

        let tabs =
            TabControl.create [
                DockPanel.dock Dock.Bottom
                TabControl.isEnabled (state.AsyncOperationInProgress |> not)
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
            ReactiveDialogHost.content (DockPanel.create [ DockPanel.margin 16; DockPanel.children [ appBar; progress; tabs ] ])
        ]

    let init store (settings: Jacqued.Settings) () =
        let events =
            seq {
                yield settings |> Settings.ConfigurationSettingsLoaded |> Msg.Settings

                yield Theme.get () |> Settings.ActualThemeSelected |> Msg.Settings

                yield! (EventStorage.readAll store) |> Seq.map Msg.Event
            }

        let noopBackupManager =
            { new IBackupManager with
                member this.backup() = Task.CompletedTask |> Async.AwaitTask
                member this.restore() = Task.CompletedTask |> Async.AwaitTask }

        let update = update store noopBackupManager

        let state =
            Seq.fold
                (fun state event -> update event state |> fst)
                { State.zero with
                    State.Settings = settings }
                events

        state, Cmd.none
