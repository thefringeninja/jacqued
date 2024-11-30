namespace Jacqued

open Expecto

[<AutoOpen>]
module Calculate =
    let bar = 20m |> (Weight >> Bar.Of)

    let platePairs =
        [ 1.25m; 2.5m; 5m; 5m; 10m; 15m; 20m; 20m ] |> List.map (Weight >> PlatePair)

    let ninetyPercentMax = 100m |> Weight

    [<Tests>]
    let set =
        testTheory
            "Calculate set"
            ([ Wave.One, RepSet.One, (65, 5u)
               Wave.One, RepSet.Two, (70, 5u)
               Wave.One, RepSet.Three, (85, 5u)
               Wave.Two, RepSet.One, (70, 3u)
               Wave.Two, RepSet.Two, (80, 3u)
               Wave.Two, RepSet.Three, (90, 3u)
               Wave.Three, RepSet.One, (75, 5u)
               Wave.Three, RepSet.Two, (85, 3u)
               Wave.Three, RepSet.Three, (95, 1u)
               Wave.Four, RepSet.One, (40, 5u)
               Wave.Four, RepSet.Two, (50, 5u)
               Wave.Four, RepSet.Three, (60, 5u) ]
             |> List.map (fun (wave, repSet, (weight, reps)) -> (wave, repSet, (weight |> Weight, reps))))
        <| fun (wave, repSet, expected) -> Expect.equal (Calculate.set wave repSet ninetyPercentMax), expected

    [<Tests>]
    let plates =
        testTheory
            "Calculate plates"
            ([ 50m, [ 15m ]; 100m, [ 20m; 20m; 15m; 5m ]; 51m, [ 15m; 1.25m ] ]
             |> List.map (fun (weight, expected) -> ((weight |> Weight), (expected |> List.map (Weight >> PlatePair)))))
        <| fun (weight, expected) -> Expect.sequenceEqual (Calculate.plates bar platePairs weight) expected
