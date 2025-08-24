module Jacqued.Workout.SupplementaryLifts

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
open Jacqued.Msg.Workout
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
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      WeightIncreases: WeightIncreases }

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
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          WeightIncreases = WeightIncreases.standard }

type private Supplement =
    | ``First Set Last``
    | ``Second Set Last``
    | ``Last Set Last``
    | ``Boring But Big (Up Down)``
    | ``Boring But Big (Ascending)``
    | ``Boring But Big (Descending)``
    | ``Boring But Strong``
    | Widowmaker

    static member all =
        [ ``First Set Last``
          ``Second Set Last``
          ``Last Set Last``
          ``Boring But Big (Up Down)``
          ``Boring But Big (Ascending)``
          ``Boring But Big (Descending)``
          ``Boring But Strong``
          Widowmaker ]

let private repify reps percentage = (percentage, reps)
let private fifteenReps p = p |> List.map (repify 15)
let private tenReps p = p |> List.map (repify 10)
let private fiveReps p = p |> List.map (repify 5)

let private bbb =
    [ (``Boring But Big (Up Down)``, [ 0.5; 0.6; 0.7; 0.6; 0.5 ] |> tenReps)
      (``Boring But Big (Ascending)``, [ 0.3; 0.4; 0.5; 0.6; 0.7 ] |> tenReps)
      (``Boring But Big (Descending)``, [ 0.7; 0.6; 0.5; 0.4; 0.3 ] |> tenReps) ]
    |> Map.ofList

let private thing supplement wave =
    match supplement with
    | ``First Set Last`` -> (Calculate.percentage wave RepSet.One) |> List.replicate 5 |> fiveReps
    | ``Second Set Last`` -> (Calculate.percentage wave RepSet.Two) |> List.replicate 5 |> fiveReps
    | ``Last Set Last`` -> (Calculate.percentage wave RepSet.Three) |> List.replicate 5 |> fiveReps
    | ``Boring But Strong`` -> (Calculate.percentage wave RepSet.One) |> List.replicate 5 |> tenReps
    | Widowmaker -> (Calculate.percentage wave RepSet.One) |> List.replicate 1 |> fifteenReps
    | s -> bbb[s]

let view (state: State) dispatch =
    let supplementary (trainingMax: Weight) supplement =
        let _, wave, _, _ = state.Mesocycles[state.CurrentExercise]
        let lifts = thing supplement wave

        let calculateWeight (repSet: int) =
            trainingMax * (lifts |> List.item repSet |> fst)

        let calculateReps (repSet: int) =
            (lifts |> List.item repSet |> snd |> uint)

        let calculatePlates = Calculate.plates state.Bar state.GymPlates

        let control repSetIndex (platePairs: PlatePair list) =
            let repSet = repSetIndex + 1
            let reps = calculateReps repSetIndex
            let weight = state.Bar.Weight + (platePairs |> List.map _.Weight |> List.sum)

            StackPanel.create [
                StackPanel.children [
                    Typography.currentExercise state.CurrentExercise
                    Typography.repSet repSet
                    Typography.weight (weight, state.MeasurementSystem)
                    Typography.reps reps

                    WrapPanel.create [
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (PlatePairs.control (state.MeasurementSystem, platePairs))
                    ]
                ]
            ]

        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children (
                lifts
                |> List.mapi (fun i _ -> i)
                |> List.map (calculateWeight >> calculatePlates)
                |> List.mapi control
                |> List.map generalize
                |> divide
            )
        ]

    let content =
        let mesocycleId, wave, oneRepMax, date = state.Mesocycles[state.CurrentExercise]

        let supplementary = supplementary oneRepMax

        let onSelectedSupplementaryLiftsIndexChanged index =
            index
            |> SupplementaryLifts.SelectedSupplementaryLiftsIndexChanged
            |> Workout.SupplementaryLifts
            |> Msg.Workout
            |> dispatch

        let onCompleteWaveClick _ =
            (mesocycleId, date, state.WeightIncreases)
            |> Workout.CompleteWave
            |> Msg.Workout
            |> dispatch

        let supplement =
            (Supplement.all, (Supplement.all |> List.map supplementary))
            ||> List.map2 (fun x y -> (x, y))

        let comboBox =
            ComboBox.create [
                ComboBox.viewItems (supplement |> List.map (fst >> string >> Typography.body2 >> generalize))
                ComboBox.onSelectedIndexChanged onSelectedSupplementaryLiftsIndexChanged
                ComboBox.selectedIndex state.SelectedIndex
            ]

        let supplement = supplement[state.SelectedIndex] |> snd

        let completeWave =
            MaterialButton.create [
                MaterialButton.content ($"Complete Wave {wave}", MaterialIconKind.Barbell)
                MaterialButton.onClick (onCompleteWaveClick, SubPatchOptions.OnChangeOf((wave, state.WeightIncreases)))
            ]

        StackPanel.create [
            StackPanel.children [
                Typography.activity "Supplements"
                comboBox
                supplement
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
        | OneRepMaxCalculated e ->
            { state with
                CurrentExercise = e.Exercise |> Exercise.next }
            |> pass
        | WeightIncreasesSet e ->
            { state with
                WeightIncreases = e.Increases }
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
    | Workout e ->
        match e with
        | WarmupLifts e ->
            match e with
            | WarmupLifts.ExerciseDateChanged date ->
                { state with
                    Mesocycles =
                        state.Mesocycles
                        |> Map.change state.CurrentExercise (function
                            | Some(mesocycleId, wave, weight, _) -> Some(mesocycleId, wave, weight, date)
                            | _ -> None) }
                |> pass
            | _ -> state |> pass

        | SupplementaryLifts e ->
            match e with
            | SelectedSupplementaryLiftsIndexChanged(selectedIndex) ->
                { state with
                    SelectedIndex = selectedIndex }
                |> pass
        | CompleteWave(mesocycleId, date, increase) ->
            state,
            handler (
                Command.CompleteWave
                    { MesocycleId = mesocycleId
                      CompletedAt = date
                      WeightIncrease = increase }
            )
        | _ -> state |> pass
    | _ -> state |> pass
