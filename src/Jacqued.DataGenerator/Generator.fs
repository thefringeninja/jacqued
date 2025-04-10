module Jacqued.Generator

open System
open Jacqued.Calculate
open Jacqued.Util

let private rand = Random()
let private bar = 20m |> (Weight >> Bar.Of)

let private plates =
    [ 1.25m; 2.5m; 5m; 5m; 10m; 15m; 20m; 20m ] |> List.map (Weight >> PlatePair)

let private setupGym =
    seq {
        "gym",
        GymSetup
            { Bar = bar
              ExercisesDaysPerWeek = ExerciseDaysPerWeek.Four
              MeasurementSystem = MeasurementSystem.Metric
              Plates = plates }
    }

let rec private nextDate date =
    seq {
        yield date
        yield! nextDate (nextExerciseDate ExerciseDaysPerWeek.Four date)
    }
let private workout count =
    let baseWeights =
        [ (Squats, 150m); (Deadlifts, 120m); (BenchPress, 100m); (OverheadPress, 105m) ]
        |> List.map (fun (e, w) -> (e, w |> Weight))
        |> Map.ofList

    let startDate =
        let today = DateOnly.today
        let daysToAdd = ((DayOfWeek.Monday |> int) - (today.DayOfWeek |> int) + 7) % 7
        today.AddDays(daysToAdd)

    let exercises =
        Exercise.all
        |> Seq.map (fun e -> e, [ 1..count ] |> List.map (fun _ -> MesocycleId.New()))
        |> Map.ofSeq

    (([ 0 .. (count - 1) ], Wave.all) ||> Seq.allPairs, Exercise.all)
    ||> Seq.allPairs
    |> Seq.zip (nextDate startDate)
    |> Seq.map (fun (startedAt, ((i, wave), exercise)) ->
        let mesocycleId = exercises[exercise][i]
        let streamName = $"mesocycle-{mesocycleId.Value:n}"

        seq {
            let increase = (if exercise.isLower then 5m else 2.5m) |> Weight

            let oneRepMax = baseWeights[exercise] + (increase * i)

            let trainingMax = oneRepMax * 0.9

            let sets =
                RepSet.all
                |> List.allPairs Wave.all
                |> List.map (fun (wave, set) ->
                    let reps = Calculate.reps wave set
                    let weight = Calculate.weight wave set bar plates trainingMax

                    ((wave, set), (weight, reps)))

                |> Map.ofList

            if wave = Wave.One then
                yield
                    streamName,
                    MesocycleStarted
                        { MeasurementSystem = MeasurementSystem.Metric
                          MesocycleId = mesocycleId
                          OneRepMax = oneRepMax
                          TrainingOneRepMax = trainingMax
                          StartedAt = startedAt
                          WorkoutPlan = { Exercise = exercise; Sets = sets } }

            yield!
                RepSet.all
                |> Seq.map (fun repSet ->
                    let weight, reps = sets[(wave, repSet)]

                    streamName,
                    RepSetCompleted
                        { Exercise = exercise
                          MesocycleId = mesocycleId
                          RepSet = repSet
                          Weight = weight
                          Wave = wave
                          Reps =
                              match repSet with
                              | RepSet.Three -> reps + (rand.Next(0, 10) |> uint32)
                              | _ -> reps
                          CompletedAt = startedAt })

            yield
                streamName,
                WaveCompleted
                    { MesocycleId = mesocycleId
                      Exercise = exercise
                      CompletedAt = startedAt
                      Wave = wave }

            if wave = Wave.Four then
                yield
                    streamName,
                    MesocycleCompleted
                        { Exercise = exercise
                          MesocycleId = mesocycleId
                          SuggestedOneRepMax = baseWeights[exercise] + (increase * (i + 1)) }
        })
    |> Seq.concat

let generate count =
    seq {
        yield! setupGym
        yield! workout count
    }
