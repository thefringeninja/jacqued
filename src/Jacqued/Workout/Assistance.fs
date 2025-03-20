module Assistance

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Avalonia.Media
open Jacqued

open Jacqued.DSL
open Jacqued.Helpers
open Material.Icons

type OneRepMaxes = Map<Exercise, Weight>

type State =
    { CurrentExercise: Exercise
      SelectedIndex: int
      Mesocycles: Map<Exercise, MesocycleId * Wave * Weight * DateTime>
      MeasurementSystem: MeasurementSystem
      Bar: Bar
      GymPlates: PlatePair list
      ColorMap: Map<Weight, Color> }

    static member zero =
        { CurrentExercise = Squats
          SelectedIndex = 0
          Mesocycles =
            Exercise.all
            |> List.map (fun e -> (e, (MesocycleId.Empty, Wave.One, Weight.zero, DateTime.MinValue)))
            |> Map.ofList
          MeasurementSystem = Metric
          Bar = Bar.Of Weight.zero
          GymPlates = []
          ColorMap = Map.empty   }

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
                    PlatePairs.control (state.MeasurementSystem, state.ColorMap, platePairs)
                ]
            ]

        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (
                [ 0..1..4 ]
                |> List.map calculateWeight
                |> List.map calculatePlates
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
                ComboBox.viewItems (assistance |> List.map fst |> List.map (Typography.body2 >> generalize))
                ComboBox.onSelectedIndexChanged onSelectedAssistanceWorkChange
                ComboBox.selectedIndex state.SelectedIndex
            ]

        let assistance = assistance[state.SelectedIndex] |> snd

        let completeWave =
            MaterialButton.create [
                Button.content ($"Complete Wave {wave}", MaterialIconKind.Barbell)
                Button.onClick (onCompleteWaveClick, SubPatchOptions.OnChangeOf(wave))
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
                GymPlates = e.Plates
                ColorMap = e.Plates |> PlatePairs.colorMap },
            List.empty |> Ok
        | MesocycleStarted e ->
            { state with
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.WorkoutPlan.Exercise (e.MesocycleId, Wave.One, e.TrainingOneRepMax, e.StartedAt) },
            List.empty |> Ok
        | RepSetCompleted e ->
            { state with CurrentExercise = e.Exercise }, List.empty |> Ok
        | WaveCompleted e ->
            let _, _, trainingMax, date = state.Mesocycles[e.Exercise]

            { state with
                Mesocycles =
                    state.Mesocycles
                    |> Map.add e.Exercise (e.MesocycleId, e.Wave |> Wave.next, trainingMax, date) },
            List.empty |> Ok
        | _ -> state, List.empty |> Ok
    | Msg.CompleteWave(mesocycleId, date) ->
        state,
        handler (
            Command.CompleteWave
                { MesocycleId = mesocycleId
                  CompletedAt = date }
        )
    | _ -> state, List.empty |> Ok
