module MainLifts

open System
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open AvaloniaDialogs.Views
open Jacqued
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Util
open Material.Icons

type WorkoutPlans = Map<Exercise, Map<Wave * RepSet, Lift>>
type Exercises = Map<Exercise, uint * MesocycleId * Wave * RepSet>

type State =
    { CurrentExercise: Exercise
      Exercises: Exercises
      WorkoutPlans: WorkoutPlans
      StartingAt: DateOnly option
      Reps: uint
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      Bar: Bar
      GymPlates: PlatePair list
      ColorMap: Map<Weight, Color> }

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
          ColorMap = Map.empty
          StartingAt = None
          Reps = 0u }

let view (state: State) dispatch =
    let mesocycleNumber, mesocycleId, wave, repSet =
        state.Exercises[state.CurrentExercise]

    let lift = state.WorkoutPlans[state.CurrentExercise][wave, repSet]

    let onCompleteRepSetClick _ =
        (mesocycleId, state.Reps, lift.Weight) |> Msg.CompleteRepSet |> dispatch

    let onExcerciseDateChange (d: Nullable<DateTimeOffset>) =
        (if d.HasValue then
             d.Value.Date |> (DateOnly.FromDateTime >> ExerciseDateChanged)
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
                    (mesocycleId, state.Reps, lift.Weight) |> Msg.FailRepSet |> dispatch

            }
            |> Async.StartImmediate)

    let onIncreaseRepsClick _ = Msg.IncreaseReps |> dispatch

    let onDecreaseRepsClick _ = Msg.DecreaseReps |> dispatch

    let increaseReps =
        MaterialButton.create [
            Button.content MaterialIconKind.ArrowUpward
            Button.onClick onIncreaseRepsClick
            Button.isEnabled (
                match lift.RepSet, state.Reps < lift.Reps with
                | RepSet.Three, _ -> true
                | _, true -> true
                | _, false -> false
            )
        ]

    let decreaseReps =
        MaterialButton.create [
            Button.content MaterialIconKind.ArrowDownward
            Button.onClick onDecreaseRepsClick
            Button.isEnabled (state.Reps > 0u)
        ]

    let completeRepSet =
        MaterialButton.create [
            Button.dock Dock.Right
            Button.content ("Complete Set", MaterialIconKind.Barbell)
            Button.onClick (onCompleteRepSetClick, SubPatchOptions.OnChangeOf(state))
            Button.isEnabled (state.Reps >= lift.Reps)
        ]

    let failRepSet =
        MaterialButton.create [
            Button.dock Dock.Left
            Button.content ("Fail Set", MaterialIconKind.CancelCircle)
            Button.onClick (onFailRepSetClick, SubPatchOptions.OnChangeOf(state))
            Button.isEnabled (state.Reps < lift.Reps)
        ]

    let plus = if lift.RepSet = RepSet.Three then "+" else ""

    let content =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                Typography.headline4 "Workout"
                Typography.headline5 $"Mesocycle {mesocycleNumber}"
                Typography.headline6 $"{state.CurrentExercise}, Wave {wave}"
                Typography.body2 $"Set {lift.RepSet}"
                Typography.body2 $"Weight: {lift.Weight}{state.MeasurementSystem}"
                Typography.body2 $"Reps: {lift.Reps}{plus}"
                DatePicker.create [
                    DatePicker.selectedDate (
                        match state.StartingAt with
                        | Some startingAt -> startingAt.DateTime
                        | _ -> DateTime.Today
                    )
                    DatePicker.horizontalAlignment HorizontalAlignment.Stretch
                    DatePicker.onSelectedDateChanged onExcerciseDateChange
                ]
                DockPanel.create [
                    DockPanel.children [
                        View.withAttrs [ Control.dock Dock.Right ] (segmentedButtonBar [ decreaseReps; increaseReps ])
                        View.withAttrs [ Control.dock Dock.Left ] (Typography.body2 $"Completed Reps: {state.Reps}")
                    ]
                ]

                PlatePairs.control (state.MeasurementSystem, state.ColorMap, lift.Plates)
                buttonBar [ completeRepSet; failRepSet ]
            ]
        ]

    layout content

let update (now: _ -> DateOnly) handler msg state =
    let nextExercise exercise = exercise |> Exercise.next

    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                GymPlates = e.Plates
                MeasurementSystem = e.MeasurementSystem
                ColorMap = e.Plates |> PlatePairs.colorMap },
            List.empty |> Ok
        | MesocycleStarted e ->
            let mesocycleNumber, _, _, _ = state.Exercises[e.WorkoutPlan.Exercise]

            { state with
                StartingAt = e.StartedAt |> Some
                CurrentExercise = e.WorkoutPlan.Exercise
                Exercises =
                    state.Exercises
                    |> Map.add e.WorkoutPlan.Exercise (mesocycleNumber + 1u, e.MesocycleId, Wave.One, RepSet.One)
                WorkoutPlans =
                    state.WorkoutPlans
                    |> Map.add
                        e.WorkoutPlan.Exercise
                        (e.WorkoutPlan.Sets
                         |> Map.toSeq
                         |> Seq.map (fun ((wave, repSet), (weight, reps)) ->
                             let plates = Calculate.plates state.Bar state.GymPlates weight

                             let lift =
                                 { Plates = plates
                                   Weight = weight
                                   Reps = reps
                                   RepSet = repSet }

                             ((wave, repSet), lift))
                         |> Map.ofSeq) },
            List.empty |> Ok
        | RepSetCompleted e ->
            let mesocycleNumber, mesocycleId, wave, _ = state.Exercises[e.Exercise]

            { state with
                Reps = 0u
                Exercises =
                    state.Exercises
                    |> Map.add e.Exercise (mesocycleNumber, mesocycleId, wave, e.RepSet |> RepSet.next) },
            List.empty |> Ok
        | WaveCompleted e ->
            let mesocycleNumber, mesocycleId, wave, _ = state.Exercises[e.Exercise]
            
            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.CompletedAt |> Some
                
                Exercises =
                    state.Exercises
                    |> Map.add e.Exercise (mesocycleNumber, mesocycleId, wave |> Wave.next, RepSet.One) },
            List.empty |> Ok
        | MesocycleFailed e ->
            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise
                StartingAt = Calculate.nextExerciseDate state.ExerciseDaysPerWeek e.FailedAt |> Some },
            List.empty |> Ok
        | MesocycleCompleted e ->
            { state with
                Reps = 0u
                CurrentExercise = e.Exercise |> nextExercise },
            List.empty |> Ok
    | ExerciseDateChanged date ->
        { state with
            State.StartingAt = date |> Some },
        List.empty |> Ok
    | StartDateChanged startingAt ->
        { state with
            StartingAt = startingAt |> Some },
        List.empty |> Ok
    | IncreaseReps ->
        { state with
            State.Reps = state.Reps + 1u },
        List.empty |> Ok
    | DecreaseReps ->
        { state with
            State.Reps = state.Reps - 1u },
        List.empty |> Ok
    | Msg.CompleteRepSet(mesocycleId, reps, weight) ->
        state,
        handler (
            Command.CompleteRepSet
                { MesocycleId = mesocycleId
                  Reps = reps
                  Weight = weight
                  CompletedAt =
                    match state.StartingAt with
                    | Some date -> date
                    | _ -> now () }
        )
    | Msg.FailRepSet(mesocycleId, reps, weight) ->
        state,
        handler (
            Command.FailRepSet
                { MesocycleId = mesocycleId
                  Reps = reps
                  Weight = weight
                  FailedAt =
                    match state.StartingAt with
                    | Some date -> date
                    | _ -> now () }
        )
    | _ -> state, List.empty |> Ok
