module Jacqued.Workout.Summary

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Jacqued
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Msg
open Material.Icons

type Exercises = Map<Exercise, uint * Wave * DateOnly>
type CompletedReps = Map<Exercise, Map<RepSet, Weight * uint>>

type OneRepMaxes = Map<Exercise, Weight>

type State =
    { CurrentExercise: Exercise
      Exercises: Exercises
      CompletedReps: CompletedReps
      OneRepMaxes: OneRepMaxes
      MeasurementSystem: MeasurementSystem }

    static member zero =
        { CurrentExercise = Squats
          Exercises =
            Exercise.all
            |> List.map (fun e -> (e, (0u, Wave.One, DateOnly.MinValue)))
            |> Map.ofList
          CompletedReps = Exercise.all |> List.map (fun e -> (e, Map.empty)) |> Map.ofList
          OneRepMaxes = Exercise.all |> List.map (fun e -> (e, Weight.zero)) |> Map.ofList
          MeasurementSystem = Metric }

let view (state: State) dispatch =
    let exercise = state.CurrentExercise |> Exercise.previous
    let mesocycleNumber, wave, completedDate = state.Exercises[exercise]
    let completedReps = state.CompletedReps[exercise]
    let wave = wave |> Wave.previous

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield Typography.activity "Summary"
                yield Typography.mesocycleNumber mesocycleNumber
                yield Typography.currentExercise (exercise, wave)
                yield Typography.date completedDate
                yield Typography.oneRepMax (state.OneRepMaxes[exercise], state.MeasurementSystem)
                yield!
                    RepSet.all
                    |> List.map (
                        (fun repSet ->
                            match (completedReps |> Map.tryFind repSet) with
                            | Some(weight, reps) ->
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Vertical
                                    StackPanel.children [
                                        Typography.repSet repSet
                                        Typography.weight (weight, state.MeasurementSystem)
                                        Typography.reps reps
                                    ]
                                ]
                            | _ -> StackPanel.create [])
                        >> generalize
                    )
                    |> divide

                let onNextExerciseClick _ =
                    exercise |> Workout.ContinueExercise |> Msg.Workout |> dispatch

                let completeWave =
                    MaterialButton.create [
                        MaterialButton.content ("Next Exercise", MaterialIconKind.ArrowRight)
                        MaterialButton.onClick onNextExerciseClick
                    ]

                yield buttonBar [ completeWave ]
            ]
        ]

    layout content

let update msg (state: State) =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                MeasurementSystem = e.MeasurementSystem }
        | OneRepMaxCalculated e ->
            { state with
                CurrentExercise = e.Exercise |> Exercise.next }
        | MesocycleStarted e ->
            let mesocycleNumber, _, _ = state.Exercises[e.WorkoutPlan.Exercise]

            { state with
                CurrentExercise = e.WorkoutPlan.Exercise
                OneRepMaxes = state.OneRepMaxes |> Map.add e.WorkoutPlan.Exercise e.OneRepMax
                Exercises =
                    state.Exercises
                    |> Map.add e.WorkoutPlan.Exercise (mesocycleNumber + 1u, Wave.One, e.StartedAt)
                CompletedReps = state.CompletedReps |> Map.add e.WorkoutPlan.Exercise Map.empty }
        | RepSetCompleted e ->
            let completedReps =
                state.CompletedReps[state.CurrentExercise]
                |> Map.add e.RepSet (e.Weight, e.Reps)

            { state with
                CompletedReps = state.CompletedReps |> Map.add state.CurrentExercise completedReps }
        | WaveCompleted e ->
            let mesocycleNumber, wave, _ = state.Exercises[e.Exercise]

            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Exercises =
                    state.Exercises
                    |> Map.add state.CurrentExercise (mesocycleNumber, wave |> Wave.next, e.CompletedAt) }
        | MesocycleFailed e ->
            let mesocycleNumber, _, _ = state.Exercises[e.Exercise]

            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Exercises =
                    state.Exercises
                    |> Map.add state.CurrentExercise (mesocycleNumber, Wave.One, e.FailedAt) }
        | _ -> state
    | _ -> state
