namespace Jacqued

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

    let reps =
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

    let set wave repSet bar platePairs (weight: Weight) =
        let weight =
            match wave, repSet with
            | Wave.One, RepSet.One -> weight * 0.65 
            | Wave.One, RepSet.Two -> weight * 0.70 
            | Wave.One, RepSet.Three -> weight * 0.85
            | Wave.Two, RepSet.One -> weight * 0.70 
            | Wave.Two, RepSet.Two -> weight * 0.80 
            | Wave.Two, RepSet.Three -> weight * 0.90
            | Wave.Three, RepSet.One -> weight * 0.75
            | Wave.Three, RepSet.Two -> weight * 0.85
            | Wave.Three, RepSet.Three -> weight * 0.95
            | Wave.Four, RepSet.One -> weight * 0.40 
            | Wave.Four, RepSet.Two -> weight * 0.50   
            | Wave.Four, RepSet.Three -> weight * 0.60 
            | _ -> Weight.zero
            
        let reps =
            match reps |> Map.tryFind (wave, repSet) with
            | Some reps -> reps
            | None -> 0u

        let platePairs = plates bar platePairs weight

        let weight = (platePairs |> List.sumBy (_.Weight)) + bar.Weight

        weight, reps

    let warmupSet repSet bar plates (weight: Weight) = set Wave.Four repSet bar plates weight

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
