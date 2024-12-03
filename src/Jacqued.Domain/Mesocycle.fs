module Jacqued.Mesocycle

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
    | Some _ -> invalidOp ""
    | None ->
        let ninetyPercent = command.OneRepMax * 0.9

        let sets =
            RepSet.all
            |> List.allPairs Wave.all
            |> List.map (fun (wave, set) -> ((wave, set), Calculate.set wave set ninetyPercent))
            |> Map.ofList

        [ MesocycleStarted
              { MesocycleId = command.MesocycleId
                MeasurementSystem = command.MeasurementSystem
                StartedAt = command.StartedAt
                OneRepMax = command.OneRepMax
                WorkoutPlan =
                  { Exercise = command.Exercise
                    Sets = sets } } ]

let private completeRepSet (command: CompleteRepSet) state =
    match state with
    | None -> invalidOp ""
    | Some state ->
        let weight =
            match state.WorkoutPlan.Sets |> Map.tryFind (state.Wave, state.RepSet) with
            | Some(weight, _) -> weight
            | None -> invalidOp ""

        [ RepSetCompleted
              { MesocycleId = state.MesocycleId
                Exercise = state.WorkoutPlan.Exercise
                Wave = state.Wave
                Reps = command.Reps
                Weight = weight
                RepSet = state.RepSet
                CompletedAt = command.CompletedAt }

          ]

let private completeWave (command: CompleteWave) state =
    match state with
    | None -> invalidOp ""
    | Some state ->
        [ if state.RepSet <> RepSet.Complete then
              invalidOp ""
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
    | None -> invalidOp ""
    | Some state ->
        let weight =
            match state.WorkoutPlan.Sets |> Map.tryFind (state.Wave, state.RepSet) with
            | Some(weight, _) -> weight
            | None -> invalidOp ""

        [ MesocycleFailed
              { MesocycleId = state.MesocycleId
                Exercise = state.WorkoutPlan.Exercise
                Wave = state.Wave
                Reps = command.Reps
                SuggestedOneRepMax = weight * 0.9
                Weight = weight
                RepSet = state.RepSet
                FailedAt = command.FailedAt } ]

let handle =
    function
    | StartMesocycle command -> start command
    | CompleteRepSet command -> completeRepSet command
    | CompleteWave command -> completeWave command
    | FailRepSet command -> failRepSet command
    | _ -> invalidOp ""

let evolve state =
    function
    | MesocycleStarted e ->
        { MesocycleId = e.MesocycleId
          Wave = Wave.One
          RepSet = RepSet.One
          WorkoutPlan = e.WorkoutPlan
          OneRepMax = e.OneRepMax
          TrainingOneRepMax = e.OneRepMax * 0.9
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
    | _ -> invalidOp ""
