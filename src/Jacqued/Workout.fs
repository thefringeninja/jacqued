﻿module Jacqued.Workout

open System
open Avalonia.Input.TextInput
open Avalonia.Media
open Avalonia.Threading
open AvaloniaDialogs.Views
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Jacqued.Assistance
open Jacqued.DSL
open Jacqued.Helpers
open Material.Icons
open Material.Styles.Controls

type Gym =
    { Bar: Bar
      PlatePairs: PlatePair list
      PlatePairColors: Map<Weight, Color>
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

type WorkoutPlans = Map<MesocycleId, WorkoutPlan>

type Screen =
    | StartMesocycle
    | WorkingOut
    | RestDay

type Lifts =
    { Exercise: Exercise
      StartingAt: DateTime option
      Wave: Wave
      RepSet: RepSet
      SelectedAssistanceWorkIndex: int
      SelectedTabIndex: int
      OneRepMax: Weight option }

    static member zero =
        { Exercise = Exercise.Squats
          StartingAt = None
          Wave = Wave.One
          RepSet = RepSet.One
          SelectedAssistanceWorkIndex = 0
          SelectedTabIndex = 0
          OneRepMax = None }

type State =
    { Gym: Gym option
      Lifts: Lifts
      Waves: Map<Exercise, Wave>
      SuggestedOneRepMaxes: Map<Exercise, Weight>
      WorkoutPlans: WorkoutPlans
      Screen: Screen }

    static member zero =
        { Gym = None
          Lifts = Lifts.zero
          Screen = Screen.StartMesocycle
          Waves = Exercise.all |> List.map (fun exercise -> (exercise, Wave.One)) |> Map.ofList
          SuggestedOneRepMaxes = Map.empty
          WorkoutPlans = Map.empty }

let findWorkoutPlan (workoutPlans: WorkoutPlans) exercise =
    workoutPlans
    |> Map.tryPick (fun mesocycleId workoutPlan ->
        if workoutPlan.Exercise = exercise then
            Some(mesocycleId, workoutPlan)
        else
            None)

let update (now: _ -> DateTime) handler msg state =
    let units, exerciseDaysPerWeek =
        match state.Gym with
        | Some gym -> gym.MeasurementSystem, gym.ExerciseDaysPerWeek
        | _ -> MeasurementSystem.Metric, ExerciseDaysPerWeek.Four

    let nextExercise exercise = exercise |> Exercise.next

    let nextScreen nextExercise =
        match findWorkoutPlan state.WorkoutPlans nextExercise with
        | Some _ -> Screen.WorkingOut
        | _ -> Screen.StartMesocycle

    let nextScreenWithRest nextScreen (date: DateTime) =
        let nextExerciseDate = Calculate.nextExerciseDay exerciseDaysPerWeek date.Date
        let now = now().Date

        if now < nextExerciseDate then
            Screen.RestDay
        else
            nextScreen

    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Gym =
                    { Bar = e.Bar
                      PlatePairs = e.Plates
                      PlatePairColors = e.Plates |> PlatePairs.colorMap
                      MeasurementSystem = e.MeasurementSystem
                      ExerciseDaysPerWeek = e.ExercisesDaysPerWeek }
                    |> Some
                State.Lifts.SelectedTabIndex = 1 },
            List.empty |> Ok
        | MesocycleStarted e ->
            { state with
                State.Screen = Screen.WorkingOut
                State.Lifts.OneRepMax = None
                State.Lifts.Wave = Wave.One
                State.Lifts.RepSet = RepSet.One
                State.Lifts.StartingAt = None
                State.Waves = state.Waves |> Map.add e.WorkoutPlan.Exercise Wave.One
                State.WorkoutPlans = state.WorkoutPlans |> Map.add e.MesocycleId e.WorkoutPlan
                State.SuggestedOneRepMaxes = state.SuggestedOneRepMaxes |> Map.add e.WorkoutPlan.Exercise e.OneRepMax },
            List.empty |> Ok
        | RepSetCompleted e ->
            let nextRepSet = e.RepSet |> RepSet.next

            { state with
                State.Lifts.RepSet = nextRepSet
                State.Lifts.SelectedTabIndex = if nextRepSet = RepSet.Complete then 2 else 1 },
            List.empty |> Ok
        | WaveCompleted e ->
            let nextExercise = state.Lifts.Exercise |> nextExercise
            let nextScreen = nextExercise |> nextScreen |> nextScreenWithRest <| e.CompletedAt

            { state with
                State.Screen = nextScreen
                State.Lifts.Exercise = nextExercise
                State.Lifts.Wave = state.Waves[nextExercise]
                State.Lifts.RepSet = RepSet.One
                State.Lifts.SelectedTabIndex = 1
                State.Waves = state.Waves |> Map.add e.Exercise (e.Wave |> Wave.next) },
            List.empty |> Ok

        | MesocycleFailed e ->
            let nextExercise = state.Lifts.Exercise |> nextExercise
            let nextScreen = nextExercise |> nextScreen |> nextScreenWithRest <| e.FailedAt

            { state with
                State.Screen = nextScreen
                State.Lifts.Exercise = nextExercise
                State.Lifts.Wave = state.Waves[nextExercise]
                State.Lifts.RepSet = RepSet.One
                State.SuggestedOneRepMaxes = state.SuggestedOneRepMaxes |> Map.add e.Exercise e.SuggestedOneRepMax
                State.Waves = state.Waves |> Map.add e.Exercise Wave.One
                State.WorkoutPlans = state.WorkoutPlans |> Map.remove e.MesocycleId },
            List.empty |> Ok
        | MesocycleCompleted e ->
            { state with
                State.SuggestedOneRepMaxes = state.SuggestedOneRepMaxes |> Map.add e.Exercise e.SuggestedOneRepMax
                State.WorkoutPlans = state.WorkoutPlans |> Map.remove e.MesocycleId },
            List.empty |> Ok
    | OneRepMaxChanged oneRepMax ->
        { state with
            State.Lifts.OneRepMax = oneRepMax |> Some },
        List.empty |> Ok
    | StartDateChanged startingAt ->
        { state with
            State.Lifts.StartingAt = startingAt |> Some },
        List.empty |> Ok
    | ExerciseDateChanged date ->
        { state with
            State.Lifts.StartingAt = date |> Some },
        List.empty |> Ok
    | SelectedAssistanceWorkIndexChanged index ->
        { state with
            State.Lifts.SelectedAssistanceWorkIndex = index },
        List.empty |> Ok
    | Msg.StartMesocycle(mesocycleId, exercise, oneRepMax, startedAt) ->
        if oneRepMax > Weight.zero then
            state,
            handler (
                Command.StartMesocycle
                    { MesocycleId = mesocycleId
                      Exercise = exercise
                      StartedAt = startedAt
                      OneRepMax = oneRepMax
                      MeasurementSystem = units }
            )
        else
            state, List.empty |> Ok
    | Msg.CompleteRepSet(mesocycleId, reps) ->
        state,
        handler (
            Command.CompleteRepSet
                { MesocycleId = mesocycleId
                  Reps = reps
                  CompletedAt =
                    match state.Lifts.StartingAt with
                    | Some date -> date
                    | _ -> now () }
        )
    | Msg.CompleteWave(mesocycleId) ->
        state,
        handler (
            Command.CompleteWave
                { MesocycleId = mesocycleId
                  CompletedAt =
                    match state.Lifts.StartingAt with
                    | Some date -> date
                    | _ -> now () }
        )
    | Msg.FailRepSet(mesocycleId, reps) ->
        state,
        handler (
            Command.FailRepSet
                { MesocycleId = mesocycleId
                  Reps = reps
                  FailedAt =
                    match state.Lifts.StartingAt with
                    | Some date -> date
                    | _ -> now () }
        )
    | Msg.ContinueExercise ->
        { state with
            State.Screen = nextScreen state.Lifts.Exercise },
        List.empty |> Ok
    | _ -> state, List.empty |> Ok

let currentOneRepMax state =
    match state.Lifts.OneRepMax with
    | Some oneRepMax -> oneRepMax
    | _ ->
        match state.SuggestedOneRepMaxes |> Map.tryFind state.Lifts.Exercise with
        | Some oneRepMax -> oneRepMax
        | _ -> Weight.zero

let startMesocycle state dispatch =
    let onOneRepMaxChange s =
        (match Weight.tryParse s with
         | Ok weight -> weight |> OneRepMaxChanged
         | Result.Error error -> error |> Message |> ApplicationError)
        |> dispatch

    let onStartDateChange (d: Nullable<DateTimeOffset>) =
        (if d.HasValue then
             d.Value.Date |> StartDateChanged
         else
             "No date selected" |> Message |> ApplicationError)
        |> dispatch

    let onStartMesocycle oneRepMax _ =
        (MesocycleId.New(),
         state.Lifts.Exercise,
         oneRepMax,
         match state.Lifts.StartingAt with
         | None -> DateTime.Today
         | Some d -> d)
        |> Msg.StartMesocycle
        |> dispatch

    let oneRepMax = currentOneRepMax state

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                TextBlock.create [ TextBlock.classes [ "Headline6" ]; TextBlock.text $"{state.Lifts.Exercise}" ]
                DatePicker.create [
                    DatePicker.selectedDate (
                        match state.Lifts.StartingAt with
                        | Some startingAt -> startingAt
                        | _ -> DateTime.Today
                    )
                    DatePicker.horizontalAlignment HorizontalAlignment.Stretch
                    DatePicker.onSelectedDateChanged onStartDateChange
                ]
                TextBox.create [
                    TextBox.label "One rep max"
                    TextBox.contentType TextInputContentType.Number
                    TextBox.text $"{oneRepMax}"
                    TextBox.onTextChanged onOneRepMaxChange
                ]
            ]
        ]

    let startMesocycle =
        FloatingButton.create [
            FloatingButton.content MaterialIconKind.Check
            FloatingButton.onClick (onStartMesocycle oneRepMax, SubPatchOptions.OnChangeOf state.Lifts)
        ]

    floatingLayout [] [ startMesocycle ] content

let currentWorkout (state: State) dispatch =
    let mesocycleId, workoutPlan =
        match findWorkoutPlan state.WorkoutPlans state.Lifts.Exercise with
        | Some workoutPlan -> workoutPlan
        | _ -> invalidOp ""

    let weight, reps =
        match workoutPlan.Sets |> Map.tryFind (state.Lifts.Wave, state.Lifts.RepSet) with
        | Some set -> set
        | _ -> invalidOp ""

    let onCompleteRepSetClick _ =
        (mesocycleId, reps) |> Msg.CompleteRepSet |> dispatch

    let onExcerciseDateChange (d: Nullable<DateTimeOffset>) =
        (if d.HasValue then
             d.Value.Date |> ExerciseDateChanged
         else
             "No date selected" |> Message |> ApplicationError)
        |> dispatch

    let onFailRepSetClick _ =
        Dispatcher.UIThread.Post(fun _ ->
            async {
                let dialog = TwofoldDialog()
                dialog.Message <- "Are you sure?"
                dialog.PositiveText <- "yes"
                dialog.NegativeText <- "no"

                let! result = dialog.ShowAsync() |> Async.AwaitTask

                if result.GetValueOrDefault() then
                    (mesocycleId, reps) |> Msg.FailRepSet |> dispatch

            }
            |> Async.StartImmediate)

    let platePairs, bar, units, colorMap =
        match state.Gym with
        | Some gym -> (Calculate.plates gym.Bar gym.PlatePairs weight), gym.Bar, gym.MeasurementSystem, gym.PlatePairColors
        | _ -> ([], Bar.Of(Weight.zero), Metric, Map.empty)

    let weight = (platePairs |> List.sumBy (_.Weight)) + bar.Weight

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                TextBlock.create [ TextBlock.classes [ "Headline6" ]; TextBlock.text $"{state.Lifts.Exercise}" ]
                DatePicker.create [
                    DatePicker.selectedDate (
                        match state.Lifts.StartingAt with
                        | Some startingAt -> startingAt
                        | _ -> DateTime.Today
                    )
                    DatePicker.horizontalAlignment HorizontalAlignment.Stretch
                    DatePicker.onSelectedDateChanged onExcerciseDateChange
                ]
                TextBlock.create [
                    TextBlock.classes [ "Subtitle1" ]
                    TextBlock.text $"Wave {state.Lifts.Wave}, Set {state.Lifts.RepSet}"
                ]
                TextBlock.create [ TextBlock.classes [ "Subtitle2" ]; TextBlock.text $"Weight: {weight}{units}" ]
                TextBlock.create [ TextBlock.classes [ "Subtitle2" ]; TextBlock.text $"Reps: {reps}" ]
                PlatePairs.control (units, colorMap, platePairs)
            ]
        ]

    let completeRepSet =
        FloatingButton.create [
            FloatingButton.content MaterialIconKind.Barbell
            FloatingButton.onClick (onCompleteRepSetClick, SubPatchOptions.OnChangeOf(state.Lifts))
        ]

    let failRepSet =
        FloatingButton.create [
            FloatingButton.content MaterialIconKind.CancelCircle
            FloatingButton.onClick (onFailRepSetClick, SubPatchOptions.OnChangeOf(state.Lifts))
        ]

    floatingLayout [] [ failRepSet; completeRepSet ] content

let warmup state _ =
    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield TextBlock.create [ TextBlock.classes [ "Headline6" ]; TextBlock.text $"{state.Lifts.Exercise}" ]
                yield!
                    RepSet.all
                    |> List.map (fun repSet ->
                        let platePairs, bar, reps, units, colorMap =
                            match state.Gym with
                            | Some gym ->
                                let oneRepMax = currentOneRepMax state

                                let weight, reps = Calculate.warmupSet repSet oneRepMax

                                (Calculate.plates gym.Bar gym.PlatePairs weight), gym.Bar, reps, gym.MeasurementSystem, gym.PlatePairColors
                            | _ -> ([], Bar.Of(Weight.zero), 0u, Metric, Map.empty)

                        let weight = (platePairs |> List.sumBy (_.Weight)) + bar.Weight

                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                TextBlock.create [ TextBlock.classes [ "Subtitle2" ]; TextBlock.text $"Weight: {weight}{units}" ]
                                TextBlock.create [ TextBlock.classes [ "Subtitle2" ]; TextBlock.text $"Reps: {reps}" ]
                                PlatePairs.control (units, colorMap, platePairs)
                            ]
                        ])
                    |> List.map generalize
                    |> divide

            ]
        ]

    floatingLayout [] [] content

let restDay _ dispatch =
    let onContinueClick _ = Msg.ContinueExercise |> dispatch

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Some inspirational text"; TextBlock.classes [ "Subtitle2" ] ]
                Button.create [ Button.content "Continue"; Button.onClick onContinueClick ]
            ]
        ]

    floatingLayout [] [] content

let assistance (state: State) dispatch =
    match state.Gym with
    | Some gym ->
        let oneRepMax =
            match state.SuggestedOneRepMaxes |> Map.tryFind state.Lifts.Exercise with
            | Some oneRepMax -> oneRepMax
            | _ -> Weight.zero

        let mesocycleId =
            match findWorkoutPlan state.WorkoutPlans state.Lifts.Exercise with
            | Some workoutPlan -> workoutPlan |> fst |> Some
            | _ -> None

        let onSelectedAssistanceWorkChange index =
            index |> Msg.SelectedAssistanceWorkIndexChanged |> dispatch

        let onCompleteWaveClick _ =
            match mesocycleId with
            | Some mesocycleId -> mesocycleId |> Msg.CompleteWave |> dispatch
            | _ -> ()

        let assistance =
            [ ("Boring But Big (Up Down)",
               (fun () ->
                   boringButBig
                       BoringButBig.UpDown
                       state.Lifts.Exercise
                       gym.Bar
                       gym.PlatePairs
                       gym.PlatePairColors
                       gym.MeasurementSystem
                       oneRepMax))
              ("Boring But Big (Descending)",
               (fun () ->
                   boringButBig
                       BoringButBig.Descending
                       state.Lifts.Exercise
                       gym.Bar
                       gym.PlatePairs
                       gym.PlatePairColors
                       gym.MeasurementSystem
                       oneRepMax))
              ("Boring But Big (Ascending)",
               (fun () ->
                   boringButBig
                       BoringButBig.Ascending
                       state.Lifts.Exercise
                       gym.Bar
                       gym.PlatePairs
                       gym.PlatePairColors
                       gym.MeasurementSystem
                       oneRepMax)) ]

        let comboBoxItem text =
            TextBlock.create [ TextBlock.text text ] |> generalize

        let comboBox =
            ComboBox.create [
                ComboBox.viewItems (assistance |> List.map fst |> List.map comboBoxItem)
                ComboBox.onSelectedIndexChanged onSelectedAssistanceWorkChange
                ComboBox.selectedIndex state.Lifts.SelectedAssistanceWorkIndex
            ]

        let assistance = assistance[state.Lifts.SelectedAssistanceWorkIndex] |> snd

        let completeWave =
            Button.create [
                Button.content $"Complete Wave {state.Lifts.Wave}"
                Button.onClick (onCompleteWaveClick, SubPatchOptions.OnChangeOf(state.Lifts))
                Button.isEnabled (state.Lifts.RepSet = RepSet.Complete)
            ]

        let content =
            StackPanel.create [ StackPanel.children [ comboBox; assistance (); completeWave ] ]

        floatingLayout [] [] content |> generalize
    | _ -> StackPanel.create []

let view state dispatch =
    TabControl.create [
        TabControl.selectedIndex state.Lifts.SelectedTabIndex
        TabControl.viewItems [
            TabItem.create [
                yield TabItem.header "Warmup"
                if state.Screen = WorkingOut then
                    yield TabItem.content (warmup state dispatch)
                else
                    yield TabItem.isEnabled false
            ]

            TabItem.create [
                yield TabItem.header "Workout"
                yield
                    TabItem.content (
                        match (state.Screen, state.Lifts.RepSet) with
                        | RestDay, _
                        | _, RepSet.Complete -> restDay state dispatch
                        | StartMesocycle, _ -> startMesocycle state dispatch
                        | WorkingOut, _ -> currentWorkout state dispatch
                    )
                if state.Lifts.RepSet = RepSet.Complete then
                    yield TabItem.isEnabled false
            ]

            TabItem.create [
                yield TabItem.header "Assistance"
                yield TabItem.content (assistance state dispatch)
            ]
        ]
    ]
