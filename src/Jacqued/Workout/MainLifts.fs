module Jacqued.Workout.MainLifts

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Styling
open Avalonia.Threading
open AvaloniaDialogs.Views
open Jacqued
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Design
open Jacqued.Helpers
open Jacqued.Msg
open Jacqued.Msg.Workout
open Jacqued.Workout.Types
open Jacqued.Util
open Material.Icons

type WorkoutPlans = Map<Exercise, Map<Wave * RepSet, Lift>>
type Exercises = Map<Exercise, uint * MesocycleId * Wave * RepSet>

type State =
    { CurrentExercise: Exercise
      Exercises: Exercises
      WorkoutPlans: WorkoutPlans
      StartingAt: DateOnly
      Reps: uint
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      Bar: Bar
      GymPlates: PlatePair list
      ActualTheme: ThemeVariant }

    static member zero =
        { MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          CurrentExercise = Squats
          Exercises =
            Exercise.all
            |> List.map (fun e -> (e, (0u, MesocycleId.Empty, Wave.One, RepSet.One)))
            |> Map.ofList
          WorkoutPlans =
            Exercise.all
            |> List.map (fun e ->
                (e,
                 (Wave.all, RepSet.all)
                 ||> Seq.allPairs
                 |> Seq.map (fun e -> (e, Lift.zero))
                 |> Map.ofSeq))
            |> Map.ofList
          Bar = Bar.zero
          GymPlates = []
          StartingAt = DateOnly.MinValue
          Reps = 0u
          ActualTheme = ThemeVariant.Default }

let view (state: State) dispatch =
    let mesocycleNumber, mesocycleId, wave, repSet =
        state.Exercises[state.CurrentExercise]

    let lift = state.WorkoutPlans[state.CurrentExercise][wave, repSet]

    let onCompleteRepSetClick _ =
        (mesocycleId, state.Reps, lift.Weight)
        |> MainLifts.CompleteRepSet
        |> Workout.MainLifts
        |> Msg.Workout
        |> dispatch

    let onFailRepSetClick _ =
        Dispatcher.UIThread.Post(fun _ ->
            async {
                let dialog =
                    TwofoldDialog(Message = "Are you sure?", PositiveText = "yes", NegativeText = "no")

                let! result = dialog.ShowAsync() |> Async.AwaitTask

                if result.GetValueOrDefault() then
                    (mesocycleId, state.Reps, lift.Weight)
                    |> MainLifts.FailRepSet
                    |> Workout.MainLifts
                    |> Msg.Workout
                    |> dispatch

            }
            |> Async.StartImmediate)

    let onIncreaseRepsClick _ =
        MainLifts.IncreaseReps |> Workout.MainLifts |> Msg.Workout |> dispatch

    let onDecreaseRepsClick _ =
        MainLifts.DecreaseReps |> Workout.MainLifts |> Msg.Workout |> dispatch

    let increaseReps =
        MaterialButton.create [
            MaterialButton.content MaterialIconKind.ArrowUpward
            MaterialButton.onClick onIncreaseRepsClick
            MaterialButton.isEnabled (
                match lift.RepSet, state.Reps < lift.Reps with
                | RepSet.Three, _ -> true
                | _, true -> true
                | _, false -> false
            )
        ]

    let decreaseReps =
        MaterialButton.create [
            MaterialButton.content MaterialIconKind.ArrowDownward
            MaterialButton.onClick onDecreaseRepsClick
            MaterialButton.isEnabled (state.Reps > 0u)
        ]

    let completeRepSet =
        MaterialButton.create [
            MaterialButton.dock Dock.Right
            MaterialButton.theme Theme.Controls.button
            MaterialButton.content ("Complete Set", MaterialIconKind.Barbell)
            MaterialButton.onClick (onCompleteRepSetClick, SubPatchOptions.OnChangeOf(state))
            MaterialButton.isEnabled (state.Reps >= lift.Reps)
        ]

    let failRepSet =
        MaterialButton.create [
            MaterialButton.dock Dock.Left
            MaterialButton.theme Theme.Controls.outlineButton
            MaterialButton.content ("Fail Set", MaterialIconKind.CancelCircle)
            MaterialButton.onClick (onFailRepSetClick, SubPatchOptions.OnChangeOf(state))
            MaterialButton.isEnabled (state.Reps < lift.Reps)
        ]

    let plus = if lift.RepSet = RepSet.Three then "+" else ""

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                Typography.headline4 "Workout"
                Typography.headline5 $"Mesocycle {mesocycleNumber}"
                Typography.headline6 $"{state.CurrentExercise}, Wave {wave}"
                Typography.subtitle1 $"{state.StartingAt:d}"
                Typography.body2 $"Set {lift.RepSet}"
                Typography.body2 $"Weight: {lift.Weight}{state.MeasurementSystem}"
                Typography.body2 $"Reps: {lift.Reps}{plus}"
                DockPanel.create [
                    DockPanel.children [
                        View.withAttrs [ Control.dock Dock.Right ] (segmentedButtonBar [ decreaseReps; increaseReps ])
                        View.withAttrs [ Control.dock Dock.Left ] (Typography.body2 $"Completed Reps: {state.Reps}")
                    ]
                ]

                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (PlatePairs.control (state.MeasurementSystem, lift.Plates))
                ]

                buttonBar [ completeRepSet; failRepSet ]
            ]
        ]

    layout content

let update handler msg state =
    let nextExercise exercise = exercise |> Exercise.next

    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                GymPlates = e.Plates
                MeasurementSystem = e.MeasurementSystem }
            |> pass
        | OneRepMaxCalculated e ->
            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.CalculatedOn }
            |> pass
        | MesocycleStarted e ->
            let mesocycleNumber, _, _, _ = state.Exercises[e.WorkoutPlan.Exercise]

            { state with
                StartingAt = e.StartedAt
                CurrentExercise = e.WorkoutPlan.Exercise
                Exercises =
                    state.Exercises
                    |> Map.add e.WorkoutPlan.Exercise (mesocycleNumber + 1u, e.MesocycleId, Wave.One, RepSet.One)
                WorkoutPlans =
                    state.WorkoutPlans
                    |> Map.add
                        e.WorkoutPlan.Exercise
                        (e.WorkoutPlan.Sets
                         |> Map.map (fun (_, repSet) (weight, reps) ->
                             { Plates = Calculate.plates state.Bar state.GymPlates weight
                               Weight = weight
                               Reps = reps
                               RepSet = repSet })) }
            |> pass
        | RepSetCompleted e ->
            let mesocycleNumber, mesocycleId, wave, _ = state.Exercises[e.Exercise]

            { state with
                Reps = 0u
                Exercises =
                    state.Exercises
                    |> Map.add e.Exercise (mesocycleNumber, mesocycleId, wave, e.RepSet |> RepSet.next) }
            |> pass
        | WaveCompleted e ->
            let mesocycleNumber, mesocycleId, wave, _ = state.Exercises[e.Exercise]

            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.CompletedAt

                Exercises =
                    state.Exercises
                    |> Map.add e.Exercise (mesocycleNumber, mesocycleId, wave |> Wave.next, RepSet.One) }
            |> pass
        | MesocycleFailed e ->
            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.FailedAt }
            |> pass
        | MesocycleCompleted e ->
            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise }
            |> pass
    | Workout e ->
        match e with
        | Mesocycle e ->
            match e with
            | StartDateChanged startingAt -> { state with StartingAt = startingAt } |> pass
            | _ -> state |> pass
        | WarmupLifts e ->
            match e with
            | WarmupLifts.ExerciseDateChanged date -> { state with State.StartingAt = date } |> pass
            | _ -> state |> pass
        | MainLifts e ->
            match e with
            | MainLifts.IncreaseReps ->
                { state with
                    State.Reps = state.Reps + 1u }
                |> pass
            | MainLifts.DecreaseReps ->
                { state with
                    State.Reps = state.Reps - 1u }
                |> pass
            | MainLifts.CompleteRepSet(mesocycleId, reps, weight) ->
                state,
                handler (
                    Command.CompleteRepSet
                        { MesocycleId = mesocycleId
                          Reps = reps
                          Weight = weight
                          CompletedAt = state.StartingAt }
                )
            | MainLifts.FailRepSet(mesocycleId, reps, weight) ->
                state,
                handler (
                    Command.FailRepSet
                        { MesocycleId = mesocycleId
                          Reps = reps
                          Weight = weight
                          FailedAt = state.StartingAt }
                )
        | _ -> state |> pass
    | Settings e ->
        match e with
        | ActualThemeSelected theme -> { state with ActualTheme = theme } |> pass
        | _ -> state |> pass
    | _ -> state |> pass
