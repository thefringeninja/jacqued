﻿namespace Jacqued

open System

module Calculate =
    let plates (bar: Bar) (platePairs: PlatePair list) (weight: Weight) : PlatePair list =
        let rec loop (acc: PlatePair list) (platePairs: PlatePair list) (weight: Weight) =
            match (platePairs, weight) with
            | _, weight when weight <= Weight.zero -> acc
            | platePair :: platePairs, _ when platePair.Weight <= weight ->
                loop (acc |> List.append [ platePair ]) platePairs (weight - platePair.Weight)
            | _ :: platePairs, _ -> loop acc platePairs weight
            | _ -> acc

        let sortedPlatePairs: PlatePair list = platePairs |> List.sort |> List.rev
        let smallestPlatePair = sortedPlatePairs |> List.last
        let remainder = weight % smallestPlatePair.Weight

        let weightRoundedUp =
            if remainder = Weight.zero then
                weight
            else
                weight + smallestPlatePair.Weight - remainder

        loop [] sortedPlatePairs (weightRoundedUp - bar.Weight) |> List.sort |> List.rev

    let private repsMap =
        [ (Wave.One, RepSet.One), 5u
          (Wave.One, RepSet.Two), 5u
          (Wave.One, RepSet.Three), 5u
          (Wave.Two, RepSet.One), 3u
          (Wave.Two, RepSet.Two), 3u
          (Wave.Two, RepSet.Three), 3u
          (Wave.Three, RepSet.One), 5u
          (Wave.Three, RepSet.Two), 3u
          (Wave.Three, RepSet.Three), 1u
          (Wave.Four, RepSet.One), 5u
          (Wave.Four, RepSet.Two), 5u
          (Wave.Four, RepSet.Three), 5u ]
        |> Map.ofList

    let reps wave repSet = repsMap[wave, repSet]

    let private weightTable =
        [ ((Wave.One, RepSet.One), 0.65)
          ((Wave.One, RepSet.Two), 0.70)
          ((Wave.One, RepSet.Three), 0.85)
          ((Wave.Two, RepSet.One), 0.70)
          ((Wave.Two, RepSet.Two), 0.80)
          ((Wave.Two, RepSet.Three), 0.90)
          ((Wave.Three, RepSet.One), 0.75)
          ((Wave.Three, RepSet.Two), 0.85)
          ((Wave.Three, RepSet.Three), 0.95)
          ((Wave.Four, RepSet.One), 0.40)
          ((Wave.Four, RepSet.Two), 0.50)
          ((Wave.Four, RepSet.Three), 0.60) ]
        |> Map.ofList

    let weight wave repSet bar platePairs (weight: Weight) =
        let weight = weight * weightTable[wave, repSet]

        let platePairs = plates bar platePairs weight

        (platePairs |> List.sumBy _.Weight) + bar.Weight

    let warmupReps =
        let reps = Wave.Four |> reps
        RepSet.all |> List.map (fun repSet -> (repSet, repSet |> reps)) |> Map.ofList

    let warmUpWeight = Wave.Four |> weight

    let nextExerciseDate (exerciseDaysPerWeek: ExerciseDaysPerWeek) (date: DateOnly) =
        match exerciseDaysPerWeek with
        | ExerciseDaysPerWeek.Three ->
            match date.DayOfWeek with
            | DayOfWeek.Monday
            | DayOfWeek.Wednesday
            | DayOfWeek.Saturday -> 2
            | DayOfWeek.Sunday
            | DayOfWeek.Tuesday
            | DayOfWeek.Thursday -> 1
            | _ -> 3
        | ExerciseDaysPerWeek.Four ->
            match date.DayOfWeek with
            | DayOfWeek.Sunday
            | DayOfWeek.Monday
            | DayOfWeek.Wednesday
            | DayOfWeek.Thursday -> 1
            | DayOfWeek.Tuesday
            | DayOfWeek.Saturday -> 2
            | _ -> 3

        |> date.AddDays
