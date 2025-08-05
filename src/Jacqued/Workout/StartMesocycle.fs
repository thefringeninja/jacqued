module Jacqued.Workout.StartMesocycle

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Input.TextInput
open Avalonia.Layout
open Jacqued
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Msg
open Jacqued.Msg.Workout
open Jacqued.Util
open Material.Icons

type Mesocycles = Map<Exercise, uint * Weight>

type State =
    { CurrentExercise: Exercise
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      MeasurementSystem: MeasurementSystem
      StartingAt: DateOnly option
      Bar: Bar
      GymPlates: PlatePair list
      Mesocycles: Mesocycles }

    static member zero =
        { CurrentExercise = Exercise.Squats
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          MeasurementSystem = MeasurementSystem.Metric
          StartingAt = None
          Bar = Bar.zero
          GymPlates = []
          Mesocycles = Exercise.all |> List.map (fun e -> (e, (1u, Weight.zero))) |> Map.ofList }

let private currentDate state =
    match state.StartingAt with
    | Some startingAt -> startingAt.DateTime
    | _ -> DateTime.Today

let view (state: State) dispatch =
    let onOneRepMaxChange s =
        (match Weight.tryParse s with
         | Ok weight -> weight |> Mesocycle.OneRepMaxChanged |> Workout.Mesocycle |> Msg.Workout
         | Result.Error error -> error |> Message |> ApplicationError)
        |> dispatch

    let onCalculateOneRepMax _ =
        state.CurrentExercise
        |> OneRepMaxLifts.BeginCalculateOneRepMaxClicked
        |> Workout.OneRepMaxLifts
        |> Msg.Workout
        |> dispatch

    let onStartDateChange (d: Nullable<DateTimeOffset>) =
        (if d.HasValue then
             d.Value.Date
             |> (DateOnly.FromDateTime
                 >> Mesocycle.StartDateChanged
                 >> Workout.Mesocycle
                 >> Msg.Workout)
         else
             "No date selected" |> Message |> ApplicationError)
        |> dispatch

    let onStartMesocycle oneRepMax _ =
        (MesocycleId.New(),
         state.CurrentExercise,
         oneRepMax,
         (match state.StartingAt with
          | None -> DateOnly.today
          | Some d -> d),
         state.Bar,
         state.GymPlates)
        |> Mesocycle.StartMesocycle
        |> Workout.Mesocycle
        |> Msg.Workout
        |> dispatch

    let calculateOneRepMax =
        FlatButton.create [
            DockPanel.dock Dock.Right
            FlatButton.content MaterialIconKind.CalculatorVariantOutline
            FlatButton.onClick (onCalculateOneRepMax, SubPatchOptions.OnChangeOf state.CurrentExercise)
        ]

    let mesocycleNumber, oneRepMax = state.Mesocycles[state.CurrentExercise]

    let startMesocycle =
        MaterialButton.create [
            MaterialButton.content ("Start Mesocycle", MaterialIconKind.Check)
            MaterialButton.onClick (onStartMesocycle oneRepMax, SubPatchOptions.OnChangeOf oneRepMax)
        ]

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                Typography.headline4 "Start Mesocycle"
                Typography.headline5 $"Mesocycle {mesocycleNumber}"
                Typography.headline6 $"{state.CurrentExercise}"
                DatePicker.create [
                    DatePicker.selectedDate (
                        match state.StartingAt with
                        | Some startingAt -> startingAt.DateTime
                        | _ -> DateTime.Today
                    )
                    DatePicker.horizontalAlignment HorizontalAlignment.Stretch
                    DatePicker.onSelectedDateChanged onStartDateChange
                ]

                DockPanel.create [
                    DockPanel.lastChildFill true
                    StackPanel.children [
                        calculateOneRepMax
                        TextBox.create [
                            DockPanel.dock Dock.Left
                            TextBox.label "1RM"
                            TextBox.contentType TextInputContentType.Number
                            TextBox.text $"{oneRepMax}"
                            TextBox.onTextChanged onOneRepMaxChange
                        ]
                    ]
                ]
                buttonBar [ startMesocycle ]
            ]
        ]

    layout content

let update now handler msg (state: State) =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                GymPlates = e.Plates
                MeasurementSystem = e.MeasurementSystem
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek }
            |> pass
        | OneRepMaxCalculated e ->
            { state with
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.CalculatedOn |> Some
                CurrentExercise = e.Exercise |> Exercise.next }
            |> pass
        | MesocycleCompleted e ->
            { state with
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise ((state.Mesocycles[e.Exercise] |> fst) + 1u, e.SuggestedOneRepMax) }
            |> pass
        | WaveCompleted e ->
            { state with
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.CompletedAt |> Some
                CurrentExercise = e.Exercise |> Exercise.next }
            |> pass
        | MesocycleFailed e ->
            { state with
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.FailedAt |> Some
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise ((state.Mesocycles[e.Exercise] |> fst) + 1u, e.SuggestedOneRepMax)
                CurrentExercise = e.Exercise |> Exercise.next }
            |> pass
        | _ -> state |> pass
    | Workout e ->
        match e with
        | Mesocycle e ->
            match e with
            | Mesocycle.OneRepMaxChanged oneRepMax ->
                { state with
                    Mesocycles =
                        state.Mesocycles
                        |> Map.add state.CurrentExercise ((state.Mesocycles[state.CurrentExercise] |> fst), oneRepMax) }
                |> pass
            | StartDateChanged startingAt ->
                { state with
                    StartingAt = startingAt |> Some }
                |> pass
            | StartMesocycle(mesocycleId, exercise, oneRepMax, startedAt, bar, platePairs) when oneRepMax > Weight.zero ->
                state,
                handler (
                    Command.StartMesocycle
                        { MesocycleId = mesocycleId
                          Exercise = exercise
                          StartedAt = startedAt
                          OneRepMax = oneRepMax
                          Bar = bar
                          Plates = platePairs
                          MeasurementSystem = state.MeasurementSystem }
                )
            | _ -> state |> pass
        | _ -> state |> pass
    | _ -> state |> pass
