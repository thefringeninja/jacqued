﻿module Jacqued.Mesocycle

type Status =
    | InProgress
    | Completed
    | Failed

type State =
    { MesocycleId: MesocycleId
      Wave: Wave
      RepSet: RepSet
      WorkoutPlan: WorkoutPlan
      OneRepMax: Weight
      TrainingOneRepMax: Weight
      MeasurementSystem: MeasurementSystem
      Status: Status }

    static member zero =
        { MesocycleId = MesocycleId.Empty
          Wave = Wave.One
          RepSet = RepSet.One
          Status = Status.InProgress
          OneRepMax = Weight.zero
          TrainingOneRepMax = Weight.zero
          MeasurementSystem = Metric
          WorkoutPlan = WorkoutPlan.zero }

let private start (command: StartMesocycle) state =
    match state with
    | Some _ -> invalidOp "Mesocycle already started"
    | None ->
        let trainingMax = command.OneRepMax * 0.9

        let sets =
            RepSet.all
            |> List.allPairs Wave.all
            |> List.map (fun (wave, set) ->
                let reps = Calculate.reps wave set
                let weight = Calculate.weight wave set command.Bar command.Plates trainingMax

                ((wave, set), (weight, reps)))

            |> Map.ofList

        [ MesocycleStarted
              { MesocycleId = command.MesocycleId
                MeasurementSystem = command.MeasurementSystem
                StartedAt = command.StartedAt
                OneRepMax = command.OneRepMax
                TrainingOneRepMax = trainingMax
                WorkoutPlan =
                  { Exercise = command.Exercise
                    Sets = sets } } ]

let mesocycleNotStarted () = invalidOp "Mesocycle not started"

let unknownSet wave repSet =
    invalidOp $"Could not find Wave {wave}, Set {repSet} in the workout plan"

let private completeRepSet (command: CompleteRepSet) state =
    match state with
    | None -> mesocycleNotStarted ()

    | Some state ->
        [ RepSetCompleted
              { MesocycleId = state.MesocycleId
                Exercise = state.WorkoutPlan.Exercise
                Wave = state.Wave
                Reps = command.Reps
                Weight = command.Weight
                RepSet = state.RepSet
                CompletedAt = command.CompletedAt } ]

let private completeWave (command: CompleteWave) state =
    match state with
    | None -> mesocycleNotStarted ()
    | Some state ->
        [ if state.RepSet <> RepSet.Complete then
              invalidOp "Expected set to be complete"
          else
              yield
                  WaveCompleted
                      { MesocycleId = state.MesocycleId
                        Exercise = state.WorkoutPlan.Exercise
                        Wave = state.Wave
                        CompletedAt = command.CompletedAt }

              if state.Wave = Wave.Four then
                  let suggestedOneRepMax =
                      state.OneRepMax
                      + match (state.MeasurementSystem, state.WorkoutPlan.Exercise) with
                        | Metric, (Squats | Deadlifts) -> Weight(5m)
                        | Metric, (BenchPress | OverheadPress) -> Weight(2.5m)
                        | Imperial, (Squats | Deadlifts) -> Weight(10m)
                        | Imperial, (BenchPress | OverheadPress) -> Weight(5m)

                  yield
                      MesocycleCompleted
                          { MesocycleId = state.MesocycleId
                            Exercise = state.WorkoutPlan.Exercise
                            SuggestedOneRepMax = suggestedOneRepMax } ]

let private failRepSet (command: FailRepSet) state =
    match state with
    | None -> mesocycleNotStarted ()
    | Some state ->
        let weight =
            match state.WorkoutPlan.Sets |> Map.tryFind (state.Wave, state.RepSet) with
            | Some(weight, _) -> weight
            | None -> unknownSet state.Wave state.RepSet

        [ MesocycleFailed
              { MesocycleId = state.MesocycleId
                Exercise = state.WorkoutPlan.Exercise
                Wave = state.Wave
                Reps = command.Reps
                SuggestedOneRepMax = weight * 0.9
                Weight = command.Weight
                RepSet = state.RepSet
                FailedAt = command.FailedAt } ]

let handle =
    function
    | StartMesocycle command -> start command
    | CompleteRepSet command -> completeRepSet command
    | CompleteWave command -> completeWave command
    | FailRepSet command -> failRepSet command
    | _ -> invalidOp "Invalid command"

let evolve state =
    function
    | MesocycleStarted e ->
        { MesocycleId = e.MesocycleId
          Wave = Wave.One
          RepSet = RepSet.One
          WorkoutPlan = e.WorkoutPlan
          OneRepMax = e.OneRepMax
          TrainingOneRepMax = e.TrainingOneRepMax
          MeasurementSystem = e.MeasurementSystem
          Status = InProgress }
    | RepSetCompleted e ->
        { state with
            RepSet = RepSet.next e.RepSet }
    | WaveCompleted e ->
        { state with
            Status = Completed
            RepSet = RepSet.One
            Wave = e.Wave |> Wave.next }

    | MesocycleFailed _ -> { state with Status = Failed }
    | _ -> state
