module Jacqued.Workout.Assistance

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Jacqued
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Util
open Material.Icons

type OneRepMaxes = Map<Exercise, Weight>

type State =
    { CurrentExercise: Exercise
      SelectedIndex: int
      Mesocycles: Map<Exercise, MesocycleId * Wave * Weight * DateOnly>
      MeasurementSystem: MeasurementSystem
      Bar: Bar
      GymPlates: PlatePair list
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

    static member zero =
        { CurrentExercise = Squats
          SelectedIndex = 0
          Mesocycles =
            Exercise.all
            |> List.map (fun e -> (e, (MesocycleId.Empty, Wave.One, Weight.zero, DateOnly.MinValue)))
            |> Map.ofList
          MeasurementSystem = Metric
          Bar = Bar.zero
          GymPlates = []
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four }

type BoringButBig =
    | UpDown
    | Ascending
    | Descending

let private subTypes =
    [ (UpDown, [ 0.5; 0.6; 0.7; 0.6; 0.5 ])
      (Ascending, [ 0.3; 0.4; 0.5; 0.6; 0.7 ])
      (Descending, [ 0.7; 0.6; 0.5; 0.4; 0.3 ]) ]
    |> Map.ofList

let view (state: State) dispatch =
    let boringButBig subType (trainingMax: Weight) =
        let calculateWeight (repSet: int) =
            trainingMax * (subTypes |> Map.find subType |> List.item repSet)

        let calculatePlates = Calculate.plates state.Bar state.GymPlates

        let control i (platePairs: PlatePair list) =
            let set = i + 1
            let weight = state.Bar.Weight + (platePairs |> List.map _.Weight |> List.sum)

            StackPanel.create [
                StackPanel.children [
                    Typography.headline6 $"{state.CurrentExercise}"
                    Typography.body2 $"Set {set}"
                    Typography.body2 $"Weight: {weight}{state.MeasurementSystem}"
                    Typography.body2 "Reps: 10"

                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (PlatePairs.control (state.MeasurementSystem, platePairs))
                    ]
                ]
            ]

        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (
                [ 0..1..4 ]
                |> List.map (calculateWeight >> calculatePlates)
                |> List.mapi control
                |> List.map generalize
                |> divide
            )
        ]

    let content =
        let mesocycleId, wave, oneRepMax, date = state.Mesocycles[state.CurrentExercise]

        let onSelectedAssistanceWorkChange index =
            index |> Msg.SelectedAssistanceWorkIndexChanged |> dispatch

        let onCompleteWaveClick _ =
            (mesocycleId, date) |> Msg.CompleteWave |> dispatch

        let assistance =
            [ ("Boring But Big (Up Down)", (fun () -> boringButBig BoringButBig.UpDown oneRepMax))
              ("Boring But Big (Descending)", (fun () -> boringButBig BoringButBig.Descending oneRepMax))
              ("Boring But Big (Ascending)", (fun () -> boringButBig BoringButBig.Ascending oneRepMax)) ]

        let comboBox =
            ComboBox.create [
                ComboBox.viewItems (assistance |> List.map (fst >> Typography.body2 >> generalize))
                ComboBox.onSelectedIndexChanged onSelectedAssistanceWorkChange
                ComboBox.selectedIndex state.SelectedIndex
            ]

        let assistance = assistance[state.SelectedIndex] |> snd

        let completeWave =
            MaterialButton.create [
                MaterialButton.content ($"Complete Wave {wave}", MaterialIconKind.Barbell)
                MaterialButton.onClick (onCompleteWaveClick, SubPatchOptions.OnChangeOf(wave))
            ]

        StackPanel.create [
            StackPanel.children [
                Typography.headline4 "Assistance"
                comboBox
                assistance ()
                buttonBar [ completeWave ]
            ]
        ]

    layout content

let update handler msg (state: State) =
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
        | MesocycleStarted e ->
            { state with
                CurrentExercise = e.WorkoutPlan.Exercise
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.WorkoutPlan.Exercise (e.MesocycleId, Wave.One, e.TrainingOneRepMax, e.StartedAt) }
            |> pass
        | WaveCompleted e ->
            let _, _, trainingMax, date = state.Mesocycles[e.Exercise]

            let date = date |> Calculate.nextExerciseWaveDate state.ExerciseDaysPerWeek

            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise (e.MesocycleId, e.Wave |> Wave.next, trainingMax, date) }
            |> pass
        | MesocycleFailed e ->
            let _, _, trainingMax, date = state.Mesocycles[e.Exercise]

            let date = date |> Calculate.nextExerciseWaveDate state.ExerciseDaysPerWeek

            { state with
                CurrentExercise = e.Exercise |> Exercise.next
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise (e.MesocycleId, e.Wave |> Wave.next, trainingMax, date) }
            |> pass

        | _ -> state |> pass
    | ExerciseDateChanged date ->
        { state with
            Mesocycles =
                state.Mesocycles
                |> Map.change state.CurrentExercise (function
                    | Some(mesocycleId, wave, weight, _) -> Some(mesocycleId, wave, weight, date)
                    | _ -> None) }
        |> pass
    | Msg.SelectedAssistanceWorkIndexChanged(selectedIndex) ->
        { state with
            SelectedIndex = selectedIndex }
        |> pass
    | Msg.CompleteWave(mesocycleId, date) ->
        state,
        handler (
            Command.CompleteWave
                { MesocycleId = mesocycleId
                  CompletedAt = date }
        )
    | _ -> state |> pass
