namespace Jacqued

open System
open Expecto
open Workout

[<AutoOpen>]
module Workout =
    let dateTime = DateTime(2000, 1, 1)

    let mesocycleIds =
        Exercise.all
        |> List.map (fun exercise -> (exercise, MesocycleId.New()))
        |> Map.ofList

    let bar = 20m |> Weight |> Bar.Of

    let platePairs =
        [ 1.25m; 2.5m; 5m; 10m; 15m; 20m ] |> List.map (Weight >> PlatePair)

    let sets =
        RepSet.all
        |> List.allPairs Wave.all
        |> List.map (fun (wave, set) ->
            let weight, reps, _ = Calculate.set wave set bar platePairs (90m |> Weight)

            ((wave, set), (weight, reps)))
        |> Map.ofList

    let gym =
        { Bar = bar
          PlatePairs = platePairs
          PlatePairColors = platePairs |> PlatePairs.colorMap
          MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four }
        |> Some

    module Messages =
        let gymSetup =
            GymSetup
                { Bar = 20m |> Weight |> Bar.Of
                  Plates = platePairs
                  MeasurementSystem = Metric
                  ExercisesDaysPerWeek = ExerciseDaysPerWeek.Four }

        let squatMesocycleStarted =
            MesocycleStarted
                { OneRepMax = 100m |> Weight
                  TrainingOneRepMax = 90m |> Weight
                  MeasurementSystem = Metric
                  MesocycleId = mesocycleIds[Exercise.Squats]
                  StartedAt = dateTime
                  WorkoutPlan = { Exercise = Squats; Sets = sets } }

        let squatReps =
            RepSet.all
            |> List.map (fun repSet ->
                let weight, reps, _ = Calculate.set Wave.One repSet bar platePairs (90m |> Weight)

                RepSetCompleted
                    { Exercise = Squats
                      Reps = reps
                      Wave = Wave.One
                      Weight = weight
                      CompletedAt = dateTime
                      MesocycleId = mesocycleIds[Squats]
                      RepSet = repSet })

    let clock () = dateTime
    let handler cmd = [] |> Result.Ok

    let update state msg = update clock handler msg state

    let acc s e = update s e |> fst

    let expect expected actual = Expect.equal actual expected ""

    let given messages =
        messages |> Seq.map Msg.Event |> Seq.fold acc State.zero

    [<Tests>]
    let gymSetup =
        test "Gym Setup" {
            [ Messages.gymSetup ]
            |> given
            |> expect
                { State.zero with
                    Gym = gym
                    Screen = Screen.StartMesocycle }
        }

    [<Tests>]
    let squatMesocycleStarted =
        test "Squat Mesocycle Started" {
            [ Messages.gymSetup; Messages.squatMesocycleStarted ]
            |> given
            |> expect
                { State.zero with
                    Gym = gym
                    SuggestedOneRepMaxes = [ (Squats, 100m |> Weight) ] |> Map.ofList
                    WorkoutPlans =
                        [ (mesocycleIds[Exercise.Squats], { Exercise = Squats; Sets = sets }) ]
                        |> Map.ofList
                    MesocycleNumbers = [ (Squats, 1u) ] |> Map.ofList
                    Lifts =
                        { Lifts.zero with
                            TrainingOneRepMax = 90m |> Weight }
                    Screen = Screen.Warmup }
        }

    [<Tests>]
    let squatWaveOneRepsCompleted =
        test "Squat Wave One Reps Completed" {
            [ [ Messages.gymSetup; Messages.squatMesocycleStarted ]; Messages.squatReps ]
            |> List.concat
            |> given
            |> expect
                { State.zero with
                    Gym = gym
                    SuggestedOneRepMaxes = [ (Squats, 100m |> Weight) ] |> Map.ofList
                    WorkoutPlans =
                        [ (mesocycleIds[Exercise.Squats], { Exercise = Squats; Sets = sets }) ]
                        |> Map.ofList
                    MesocycleNumbers = [ (Squats, 1u) ] |> Map.ofList
                    Lifts =
                        { Lifts.zero with
                            TrainingOneRepMax = 90m |> Weight
                            RepSet = RepSet.Complete
                            CompletedReps =
                                RepSet.all
                                |> List.map (fun repSet ->
                                    let weight, reps, _ = Calculate.set Wave.One repSet bar platePairs (90m |> Weight)
                                    (repSet, (weight, reps)))
                                |> Map.ofList }
                    Screen = Screen.Assistance }
        }
