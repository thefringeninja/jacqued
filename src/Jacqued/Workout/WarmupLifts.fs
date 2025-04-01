module WarmupLifts

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Jacqued
open Jacqued.DSL
open Jacqued.Helpers
open Material.Icons

type Exercises = Map<Exercise, uint * Wave * Lift list>
type State =
    { CurrentExercise: Exercise
      Exercises: Exercises
      Date: DateOnly
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      Bar: Bar
      GymPlates: PlatePair list
      ColorMap: Map<Weight, Color> }

    static member zero =
        { MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          CurrentExercise = Squats
          Exercises = Exercise.all |> List.map (fun e -> (e, (0u, Wave.One, []))) |> Map.ofList
          Bar = Bar.zero
          GymPlates = []
          ColorMap = Map.empty
          Date = DateOnly.MinValue }

let view (state: State) dispatch =
    let mesocycleNumber, wave, set = state.Exercises[state.CurrentExercise]

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield Typography.headline4 "Warmup"
                yield Typography.headline5 $"Mesocycle {mesocycleNumber}"
                yield Typography.headline6 $"{state.CurrentExercise}, Wave {wave}"
                yield Typography.subtitle1 $"{state.Date:d}"

                yield!
                    set
                    |> List.map (fun set ->
                        StackPanel.create [

                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [
                                Typography.body2 $"Set {set.RepSet}"
                                Typography.body2 $"Weight: {set.Weight}{state.MeasurementSystem}"
                                Typography.body2 $"Reps: {set.Reps}"
                                PlatePairs.control (state.MeasurementSystem, state.ColorMap, set.Plates)
                            ]
                        ])
                    |> List.map generalize
                    |> divide

                let onCompleteWarmupClick _ = Msg.CompleteWarmup |> dispatch

                let completeWarmup =
                    MaterialButton.create [
                        Button.content ("Complete Warmup", MaterialIconKind.Barbell)
                        Button.onClick onCompleteWarmupClick
                    ]

                yield buttonBar [ completeWarmup ]
            ]
        ]

    layout content

let update msg (state: State) =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                GymPlates = e.Plates
                MeasurementSystem = e.MeasurementSystem
                ColorMap = e.Plates |> PlatePairs.colorMap }
        | MesocycleStarted e ->
            let mesocycleNumber, _, _ = state.Exercises[e.WorkoutPlan.Exercise]

            let calculateWarmupSet repSet =
                let reps = Calculate.warmupReps[repSet]
                let weight =
                    Calculate.warmUpWeight repSet state.Bar state.GymPlates e.TrainingOneRepMax

                let plates = Calculate.plates state.Bar state.GymPlates weight
                { Weight = weight
                  Reps = reps
                  Plates = plates
                  RepSet = repSet }

            { state with
                CurrentExercise = e.WorkoutPlan.Exercise
                Date = e.StartedAt
                Exercises =
                    state.Exercises
                    |> Map.add e.WorkoutPlan.Exercise (mesocycleNumber + 1u, Wave.One, (RepSet.all |> List.map calculateWarmupSet)) }
        | WaveCompleted e ->
            let mesocycleNumber, _, lifts = state.Exercises[e.Exercise]
            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Date = e.CompletedAt |> Calculate.nextExerciseDate state.ExerciseDaysPerWeek
                Exercises = state.Exercises |> Map.add e.Exercise (mesocycleNumber, e.Wave |> Wave.next, lifts)  }
        | MesocycleFailed e ->
            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Date = e.FailedAt |> Calculate.nextExerciseDate state.ExerciseDaysPerWeek }
        | _ -> state

    | _ -> state
