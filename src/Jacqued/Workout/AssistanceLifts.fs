module Jacqued.Workout.AssistanceLifts

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Jacqued
open Jacqued.Calculate
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Msg
open Jacqued.Msg.Workout
open Jacqued.Util
open Material.Icons

type OneRepMaxes = Map<Exercise, Weight>

type State =
    { CurrentExercise: Exercise
      SelectedAssistanceTemplate: (AssistanceTemplateId * string) option
      AssistanceTemplates: Map<AssistanceTemplateId, string>
      AssistanceExercises: AssistanceExercise list
      Mesocycles: Map<Exercise, MesocycleId * uint * Wave * Weight * DateOnly>
      MeasurementSystem: MeasurementSystem
      Bar: Bar
      GymPlates: PlatePair list
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      WeightIncreases: WeightIncreases }

    static member zero =
        { CurrentExercise = Squats
          SelectedAssistanceTemplate = None
          AssistanceTemplates = Map.empty
          AssistanceExercises = List.empty
          Mesocycles =
            Exercise.all
            |> List.map (fun e -> (e, (MesocycleId.Empty, 1u, Wave.One, Weight.zero, DateOnly.MinValue)))
            |> Map.ofList
          MeasurementSystem = Metric
          Bar = Bar.zero
          GymPlates = []
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          WeightIncreases = WeightIncreases.standard }

let update handler (getAssistanceExercises: AssistanceTemplateId -> Exercise -> AssistanceExercise list) msg (state: State) =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                MeasurementSystem = e.MeasurementSystem
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                GymPlates = e.Plates }
            |> pass
        | OneRepMaxCalculated e ->
            { state with
                CurrentExercise = e.Exercise |> Exercise.next }
            |> pass
        | WeightIncreasesSet e ->
            { state with
                WeightIncreases = e.Increases }
            |> pass
        | MesocycleStarted e ->
            let _, mesocycleNumber, _, _, _ = state.Mesocycles[e.WorkoutPlan.Exercise]

            { state with
                CurrentExercise = e.WorkoutPlan.Exercise
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.WorkoutPlan.Exercise (e.MesocycleId, mesocycleNumber + 1u, Wave.One, e.OneRepMax, e.StartedAt) }
            |> pass
        | WaveCompleted e ->
            let _, mesocycleNumber, _, oneRepMax, date = state.Mesocycles[e.Exercise]

            let date = date |> Calculate.nextExerciseWaveDate state.ExerciseDaysPerWeek

            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise (e.MesocycleId, mesocycleNumber, e.Wave |> Wave.next, oneRepMax, date) }
            |> pass
        | MesocycleFailed e ->
            let _, mesocycleNumber, _, oneRepMax, date = state.Mesocycles[e.Exercise]

            let date = date |> Calculate.nextExerciseWaveDate state.ExerciseDaysPerWeek

            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise (e.MesocycleId, mesocycleNumber + 1u, e.Wave |> Wave.next, oneRepMax, date) }
            |> pass
        | AssistanceTemplateDefined e ->
            { state with
                AssistanceTemplates = state.AssistanceTemplates |> Map.add e.AssistanceTemplateId e.Name }
            |> pass
        | AssistanceTemplateRemoved e ->
            { state with
                AssistanceTemplates = state.AssistanceTemplates |> Map.remove e.AssistanceTemplateId }
            |> pass
        | _ -> state |> pass
    | Msg.Workout e ->
        match e with
        | AssistanceLifts e ->
            match e with
            | SelectedAssistanceExerciseTemplateChanged(assistanceTemplateId, name) ->
                { state with
                    AssistanceExercises = getAssistanceExercises assistanceTemplateId state.CurrentExercise
                    SelectedAssistanceTemplate = (assistanceTemplateId, name) |> Some }
                |> pass
        | WarmupLifts e ->
            match e with
            | WarmupLifts.ExerciseDateChanged date ->
                { state with
                    Mesocycles =
                        state.Mesocycles
                        |> Map.change state.CurrentExercise (function
                            | Some(mesocycleId, mesocycleNumber, wave, oneRepMax, _) ->
                                Some(mesocycleId, mesocycleNumber, wave, oneRepMax, date)
                            | _ -> None) }
                |> pass
            | _ -> state |> pass
        | CompleteWave(mesocycleId, date, increase) ->
            (state,
             handler (
                 Command.CompleteWave
                     { MesocycleId = mesocycleId
                       CompletedAt = date
                       WeightIncrease = increase }
             ))
            |> cmd
        | _ -> state |> pass
    | _ -> state |> pass

let view (state: State) dispatch =
    let mesocycleId, mesocycleNumber, wave, oneRepMax, date =
        state.Mesocycles[state.CurrentExercise]

    let onCompleteWaveClick _ =
        (mesocycleId, date, state.WeightIncreases)
        |> Workout.CompleteWave
        |> Msg.Workout
        |> dispatch

    let completeWave =
        MaterialButton.create [
            MaterialButton.content ($"Complete Wave {wave}", MaterialIconKind.Barbell)
            MaterialButton.onClick (onCompleteWaveClick, SubPatchOptions.OnChangeOf((wave, state.WeightIncreases)))
        ]

    let control =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield Typography.activity "Assistance"
                yield Typography.mesocycleNumber mesocycleNumber
                yield Typography.currentExercise (state.CurrentExercise, wave)
                yield Typography.date date
                yield Typography.oneRepMax (oneRepMax, state.MeasurementSystem)

                yield
                    ComboBox.create [
                        ComboBox.label "Assistance Template"
                        ComboBox.dataItems (state.AssistanceTemplates |> Map.toList)
                        ComboBox.itemTemplate (
                            DataTemplateView<AssistanceTemplateId * string>.create (snd >> string >> Typography.body2 >> centerComboBoxItem)
                        )
                        ComboBox.selectedItem state.SelectedAssistanceTemplate
                        ComboBox.onSelectedItemChanged (
                            (fun o ->
                                (o :?> AssistanceTemplateId * string)
                                |> AssistanceLifts.SelectedAssistanceExerciseTemplateChanged
                                |> Workout.AssistanceLifts
                                |> Msg.Workout
                                |> dispatch)
                        )
                    ]

                yield!
                    state.AssistanceExercises
                    |> List.map (fun ae ->
                        let _, _, _, oneRepMax, _ = state.Mesocycles[ae.BaseExercise]
                        let platePairs = ae.calculate state.Bar state.GymPlates oneRepMax

                        let weight = (platePairs |> List.sumBy _.Weight) + state.Bar.Weight

                        StackPanel.create [
                            StackPanel.children [
                                Typography.subtitle1 ae.Name
                                Typography.weight (weight, state.MeasurementSystem)
                                Typography.body2 ae.Description
                                Typography.body2 $"{ae.Sets}x{ae.Reps}"

                                WrapPanel.create [
                                    WrapPanel.orientation Orientation.Horizontal
                                    WrapPanel.children (PlatePairs.control (state.MeasurementSystem, platePairs))
                                ]
                            ]
                        ]
                        |> generalize)

                yield buttonBar [ completeWave ]
            ]
        ]

    layout control
