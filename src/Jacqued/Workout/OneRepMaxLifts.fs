module Jacqued.Workout.OneRepMaxLifts

open System

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Input.TextInput
open Avalonia.Layout
open Avalonia.Styling
open Jacqued
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Msg
open Jacqued.Msg.Workout
open Jacqued.Util
open Material.Icons

type OneRepMaxes = Map<Exercise, Weight>

type Screen =
    | EstimateOneRepMax
    | Warmup
    | CalculateOneRepMax
    | Summary

type State =
    { CurrentExercise: Exercise
      Date: DateOnly
      Reps: uint
      EstimatedOneRepMax: Weight
      OneRepMaxes: OneRepMaxes
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      Bar: Bar
      GymPlates: PlatePair list
      Plates: PlatePair list
      TestWeight: Weight
      ActualTheme: ThemeVariant
      Screen: Screen }

    static member zero =
        { MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          CurrentExercise = Squats
          EstimatedOneRepMax = Weight.zero
          Bar = Bar.zero
          GymPlates = []
          Plates = []
          TestWeight = Weight.zero
          Date = DateOnly.MinValue
          Reps = 0u
          OneRepMaxes = Exercise.all |> List.map (fun e -> (e, Weight.zero)) |> Map.ofList
          ActualTheme = ThemeVariant.Default
          Screen = Screen.EstimateOneRepMax }

let update handler msg state =
    let calculate (weight: Weight) =
        let testWeight = weight * 0.8
        let plates = Calculate.plates state.Bar state.GymPlates testWeight
        let testWeight = Calculate.equipmentWeight state.Bar plates testWeight
        testWeight, plates

    let nextExerciseDate = Calculate.nextExerciseDate state.ExerciseDaysPerWeek

    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                Bar = e.Bar
                MeasurementSystem = e.MeasurementSystem
                GymPlates = e.Plates }
            |> pass
        | OneRepMaxCalculated e ->
            { state with
                Date = e.CalculatedOn |> nextExerciseDate
                OneRepMaxes = state.OneRepMaxes |> Map.add e.Exercise e.OneRepMax
                Screen = Screen.Summary }
            |> pass
        | MesocycleStarted e ->
            { state with
                Date = e.StartedAt
                OneRepMaxes = state.OneRepMaxes |> Map.add e.WorkoutPlan.Exercise e.OneRepMax }
            |> pass
        | MesocycleFailed e ->
            { state with
                Date = e.FailedAt |> nextExerciseDate
                OneRepMaxes = state.OneRepMaxes |> Map.add e.Exercise e.SuggestedOneRepMax }
            |> pass
        | WaveCompleted e ->
            { state with
                Date = e.CompletedAt |> nextExerciseDate }
            |> pass
        | MesocycleCompleted e ->
            { state with
                OneRepMaxes = state.OneRepMaxes |> Map.add e.Exercise e.SuggestedOneRepMax }
            |> pass
        | _ -> state |> pass
    | Workout e ->
        match e with
        | OneRepMaxLifts e ->
            match e with
            | BeginCalculateOneRepMaxClicked exercise ->
                let estimatedOneRepMax =
                    match state.OneRepMaxes |> Map.tryFind exercise with
                    | Some weight -> weight
                    | _ -> Weight.zero

                { state with
                    Reps = 0u
                    CurrentExercise = exercise
                    EstimatedOneRepMax = estimatedOneRepMax
                    Screen = Screen.EstimateOneRepMax }
                |> pass
            | ExerciseDateChanged date -> { state with Date = date } |> pass
            | OneRepMaxChanged weight ->
                let testWeight, plates = calculate weight

                { state with
                    State.EstimatedOneRepMax = weight
                    Plates = plates
                    TestWeight = testWeight }
                |> pass
            | OneRepMaxEstimated ->
                let testWeight, plates = calculate state.EstimatedOneRepMax

                { state with
                    Plates = plates
                    TestWeight = testWeight
                    Screen = Screen.Warmup }
                |> pass
            | CompleteWarmup ->
                { state with
                    Screen = Screen.CalculateOneRepMax }
                |> pass
            | IncreaseReps ->
                { state with
                    State.Reps = state.Reps + 1u }
                |> pass
            | DecreaseReps ->
                { state with
                    State.Reps = state.Reps - 1u }
                |> pass
            | CompleteCalculateOneRepMaxClicked(exercise, reps, weight, calculatedOn) ->
                state,
                handler (
                    Command.CalculateOneRepMax
                        { Exercise = exercise
                          Reps = reps
                          Weight = weight
                          CalculatedOn = calculatedOn }
                )
            | _ -> state |> pass
        | _ -> state |> pass
    | _ -> state |> pass

let estimate (state: State) dispatch =
    let onOneRepMaxChange s =
        (match Weight.tryParse s with
         | Ok weight ->
             weight
             |> OneRepMaxLifts.OneRepMaxChanged
             |> Workout.OneRepMaxLifts
             |> Msg.Workout
         | Result.Error error -> error |> Message |> ApplicationError)
        |> dispatch

    let oneRepMaxEstimate =
        TextBox.create [
            DockPanel.dock Dock.Left
            TextBox.label "1RM"
            TextBox.contentType TextInputContentType.Number
            TextBox.text $"{state.EstimatedOneRepMax}"
            TextBox.onTextChanged onOneRepMaxChange
        ]

    let onEstimateOneRepMaxClick _ =
        OneRepMaxLifts.OneRepMaxEstimated
        |> Workout.OneRepMaxLifts
        |> Msg.Workout
        |> dispatch

    let estimateOneRepMax =
        MaterialButton.create [
            MaterialButton.content ("Next", MaterialIconKind.ArrowRight)
            MaterialButton.onClick onEstimateOneRepMaxClick
        ]

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                Typography.activity "Estimate 1RM"
                Typography.currentExercise state.CurrentExercise
                Typography.date state.Date
                oneRepMaxEstimate
                buttonBar [ estimateOneRepMax ]
            ]
        ]

    layout content

let calculate (state: State) dispatch =
    let onIncreaseRepsClick _ =
        OneRepMaxLifts.IncreaseReps |> Workout.OneRepMaxLifts |> Msg.Workout |> dispatch

    let onDecreaseRepsClick _ =
        OneRepMaxLifts.DecreaseReps |> Workout.OneRepMaxLifts |> Msg.Workout |> dispatch

    let onCalculateOneRepMaxClick _ =
        (state.CurrentExercise, state.Reps, state.EstimatedOneRepMax, state.Date)
        |> OneRepMaxLifts.CompleteCalculateOneRepMaxClicked
        |> Workout.OneRepMaxLifts
        |> Msg.Workout
        |> dispatch

    let increaseReps =
        MaterialButton.create [
            MaterialButton.content MaterialIconKind.ArrowUpward
            MaterialButton.onClick onIncreaseRepsClick
        ]

    let decreaseReps =
        MaterialButton.create [
            MaterialButton.content MaterialIconKind.ArrowDownward
            MaterialButton.onClick onDecreaseRepsClick
            MaterialButton.isEnabled (state.Reps > 0u)
        ]

    let completeCalculateOneRepMax =
        MaterialButton.create [
            MaterialButton.content ("Complete Set", MaterialIconKind.Barbell)
            MaterialButton.onClick onCalculateOneRepMaxClick
        ]

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                Typography.activity "Calculate 1RM"
                Typography.currentExercise state.CurrentExercise
                Typography.weight (state.TestWeight, state.MeasurementSystem)
                DockPanel.create [
                    DockPanel.children [
                        View.withAttrs [ Control.dock Dock.Right ] (segmentedButtonBar [ decreaseReps; increaseReps ])
                        View.withAttrs [ Control.dock Dock.Left ] (Typography.completedReps state.Reps)
                    ]
                ]

                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (PlatePairs.control (state.MeasurementSystem, state.Plates))
                ]

                buttonBar [ completeCalculateOneRepMax ]
            ]
        ]

    layout content

let warmup state dispatch =
    let calculateWarmupSet repSet =
        let reps = Calculate.warmupReps[repSet]

        let weight =
            Calculate.warmUpWeight repSet state.Bar state.GymPlates state.EstimatedOneRepMax

        let plates = Calculate.plates state.Bar state.GymPlates weight

        {| Weight = weight
           Reps = reps
           Plates = plates
           RepSet = repSet |}

    let set = RepSet.all |> List.map calculateWarmupSet

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield Typography.activity "Warmup"
                yield Typography.currentExercise state.CurrentExercise
                yield!
                    set
                    |> List.map (
                        (fun set ->
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children [
                                    Typography.repSet set.RepSet
                                    Typography.weight (set.Weight, state.MeasurementSystem)
                                    Typography.reps set.Reps
                                    WrapPanel.create [
                                        WrapPanel.orientation Orientation.Horizontal
                                        WrapPanel.children (PlatePairs.control (state.MeasurementSystem, set.Plates))
                                    ]
                                ]
                            ])
                        >> generalize
                    )
                    |> divide

                let onCompleteWarmupClick _ =
                    OneRepMaxLifts.CompleteWarmup
                    |> Workout.OneRepMaxLifts
                    |> Msg.Workout
                    |> dispatch

                let completeWarmup =
                    MaterialButton.create [
                        MaterialButton.content ("Complete Warmup", MaterialIconKind.Barbell)
                        MaterialButton.onClick onCompleteWarmupClick
                    ]

                yield buttonBar [ completeWarmup ]
            ]
        ]

    layout content

let summary state dispatch =
    let oneRepMax =
        match state.OneRepMaxes |> Map.tryFind state.CurrentExercise with
        | Some weight -> weight
        | None -> Weight.zero

    let onNextExerciseClick _ =
        state.CurrentExercise
        |> OneRepMaxLifts.Complete
        |> Workout.OneRepMaxLifts
        |> Msg.Workout
        |> dispatch

    let nextExercise =
        MaterialButton.create [
            MaterialButton.content ("Next Exercise", MaterialIconKind.ArrowRight)
            MaterialButton.onClick onNextExerciseClick
        ]

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield Typography.activity "Summary"
                yield Typography.currentExercise state.CurrentExercise
                yield Typography.date state.Date
                yield Typography.oneRepMax (oneRepMax, state.MeasurementSystem)

                yield buttonBar [ nextExercise ]
            ]
        ]

    layout content

let view state dispatch =
    (function
    | EstimateOneRepMax -> estimate state
    | Warmup -> warmup state
    | CalculateOneRepMax -> calculate state
    | Summary -> summary state)
        state.Screen
        dispatch
