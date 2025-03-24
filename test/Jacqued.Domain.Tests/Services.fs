namespace Jacqued

open System
open Expecto

[<AutoOpen>]
module Calculate =
    let bar = 20m |> (Weight >> Bar.Of)

    let platePairs =
        [ 1.25m; 2.5m; 5m; 5m; 10m; 15m; 20m; 20m ] |> List.map (Weight >> PlatePair)

    let ninetyPercentMax = 100m |> Weight

    [<Tests>]
    let set =
        testList "Calculate set" [
            testTheory
                "Calculate set"
                ([ Wave.One, RepSet.One, (65, 5u, [ 20m; 2.5m ])
                   Wave.One, RepSet.Two, (70, 5u, [ 20m; 5m ])
                   Wave.One, RepSet.Three, (85, 5u, [ 20m; 10m; 2.5m ])
                   Wave.Two, RepSet.One, (70, 3u, [ 20m; 5m ])
                   Wave.Two, RepSet.Two, (80, 3u, [ 20m; 10m ])
                   Wave.Two, RepSet.Three, (90, 3u, [ 20m; 15m ])
                   Wave.Three, RepSet.One, (75, 5u, [ 20m; 5m; 2.5m ])
                   Wave.Three, RepSet.Two, (85, 3u, [ 20m; 10m; 2.5m ])
                   Wave.Three, RepSet.Three, (95, 1u, [ 20m; 15m; 2.5m ])
                   Wave.Four, RepSet.One, (40, 5u, [ 10m ])
                   Wave.Four, RepSet.Two, (50, 5u, [ 15m ])
                   Wave.Four, RepSet.Three, (60, 5u, [ 20m ]) ]
                 |> List.map (fun (wave, repSet, (weight, reps, plates)) ->
                     (wave, repSet, (weight |> Weight, reps, plates |> List.map (Weight >> PlatePair)))))
            <| fun (wave, repSet, expected) -> Expect.equal (Calculate.set wave repSet bar platePairs ninetyPercentMax) expected ""
        ]

    [<Tests>]
    let plates =
        testTheory
            "Calculate plates"
            ([ 50m, [ 15m ]; 100m, [ 20m; 20m ]; 51m, [ 15m; 1.25m ] ]
             |> List.map (fun (weight, expected) -> ((weight |> Weight), (expected |> List.map (Weight >> PlatePair)))))
        <| fun (weight, expected) -> Expect.sequenceEqual (Calculate.plates bar platePairs weight) expected ""

    [<Tests>]
    let nextExerciseDate =
        testTheory
            "Calculate next exercise date"
            ([ ExerciseDaysPerWeek.Three, "11-24", "11-25"
               ExerciseDaysPerWeek.Three, "11-25", "11-27"
               ExerciseDaysPerWeek.Three, "11-26", "11-27"
               ExerciseDaysPerWeek.Three, "11-27", "11-29"
               ExerciseDaysPerWeek.Three, "11-28", "11-29"
               ExerciseDaysPerWeek.Three, "11-29", "12-02"
               ExerciseDaysPerWeek.Three, "11-30", "12-02"
               ExerciseDaysPerWeek.Four, "11-24", "11-25"
               ExerciseDaysPerWeek.Four, "11-25", "11-26"
               ExerciseDaysPerWeek.Four, "11-26", "11-28"
               ExerciseDaysPerWeek.Four, "11-27", "11-28"
               ExerciseDaysPerWeek.Four, "11-28", "11-29"
               ExerciseDaysPerWeek.Four, "11-29", "12-02"
               ExerciseDaysPerWeek.Four, "11-30", "12-02" ]
             |> List.map (fun (e, d, ex) ->
                 (e, DateOnly.ParseExact($"2024-{d}", "yyyy-MM-dd", null), DateOnly.ParseExact($"2024-{ex}", "yyyy-MM-dd", null))))
        <| fun (exerciseDaysPerWeek, date, expected) -> Expect.equal (Calculate.nextExerciseDate exerciseDaysPerWeek date) expected ""
