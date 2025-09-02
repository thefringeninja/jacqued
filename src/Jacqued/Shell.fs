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
        let readBackwards = EventStorage.readStreamBackward store

        let gym = Gym.create read append

        let exercises = Exercises.create append
        let mesocycle = Mesocycle.create read append

        let assistanceTemplate, getAssistanceTemplate =
            AssistanceTemplate.create readBackwards append (fun name ->
                state.Setup.AssistanceTemplate.AssistanceTemplates
                |> Map.exists (fun _ n -> n = name))

        let workout command =
            match command with
            | CalculateOneRepMax _ -> exercises command
            | _ -> mesocycle command

        let setup command =
            match command with
            | SetWeightIncreases _ -> exercises command
            | DefineAssistanceTemplate _ -> assistanceTemplate command
            | RemoveAssistanceTemplate _ -> assistanceTemplate command
            | _ -> gym command

        let setupComplete =
            match msg with
            | Event e ->
                match e with
                | GymSetup _ -> true
                | _ -> state.SetupComplete
            | _ -> state.SetupComplete

        let batch acc (state, cmd) = (state, [ cmd; acc ] |> Cmd.batch)

        let state, cmd =
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

                state, Cmd.none
            | Msg.Data msg ->
                match msg with
                | Data.BeginBackup ->
                    { state with
                        AsyncOperationInProgress = true },
                    Cmd.OfAsync.either
                        backupManager.backup
                        ()
                        (Ok >> Data.CompleteBackup >> Msg.Data)
                        (exnToError >> Data.CompleteBackup >> Msg.Data)
                | Data.CompleteBackup msg ->
                    { state with
                        AsyncOperationInProgress = false },
                    match msg with
                    | Ok _ -> Cmd.none
                    | Error error -> error |> Message |> Msg.ApplicationError |> Cmd.ofMsg
                | Data.BeginRestore ->
                    { state with
                        AsyncOperationInProgress = true },
                    Cmd.OfAsync.either
                        backupManager.restore
                        ()
                        (Ok >> Data.CompleteRestore >> Msg.Data)
                        (exnToError >> Data.CompleteRestore >> Msg.Data)
                | Data.CompleteRestore msg ->
                    let state, cmd =
                        match msg with
                        | Ok _ ->
                            Seq.fold
                                (fun state event -> (update store backupManager event state) |> fst)
                                { State.zero with
                                    State.Settings = state.Settings }
                                ((EventStorage.readAll store) |> Seq.map Msg.Event),
                            Cmd.none
                        | Error error -> state, error |> Message |> Msg.ApplicationError |> Cmd.ofMsg

                    { state with
                        AsyncOperationInProgress = false },
                    cmd
            | _ ->
                try
                    let setup, cmd =
                        (Setup.update setup getAssistanceTemplate msg state.Setup) |> batch Cmd.none

                    let progress = Progress.update msg state.Progress

                    let getAssistanceExercise assistanceTemplateId exercise =
                        (assistanceTemplateId |> getAssistanceTemplate).Exercises[exercise]

                    let workout, cmd =
                        (Workout.update (fun () -> DateOnly.today) getAssistanceExercise workout msg state.Workout)
                        |> batch cmd

                    let settings, cmd = (Configuration.update msg state.Settings) |> batch cmd

                    { state with
                        Setup = setup
                        Progress = progress
                        Workout = workout
                        Settings = settings },
                    cmd
                with exn ->
                    state, exn |> ApplicationError.Exception |> Msg.ApplicationError |> Cmd.ofMsg

        { state with
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
