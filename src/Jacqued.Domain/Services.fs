namespace Jacqued

module Calculate =
    let set wave repSet (weight: Weight) =
        match wave, repSet with
        | Wave.One, RepSet.One -> weight * 0.75, 5u
        | Wave.One, RepSet.Two -> weight * 0.80, 5u
        | Wave.One, RepSet.Three -> weight * 0.85, 5u
        | Wave.Two, RepSet.One -> weight * 0.80, 3u
        | Wave.Two, RepSet.Two -> weight * 0.85, 3u
        | Wave.Two, RepSet.Three -> weight * 0.90, 3u
        | Wave.Three, RepSet.One -> weight * 0.75, 5u
        | Wave.Three, RepSet.Two -> weight * 0.85, 3u
        | Wave.Three, RepSet.Three -> weight * 0.95, 1u
        | Wave.Four, RepSet.One -> weight * 0.60, 5u
        | Wave.Four, RepSet.Two -> weight * 0.65, 5u
        | Wave.Four, RepSet.Three -> weight * 0.70, 5u

    let warmupSet repSet (weight: Weight) = set Wave.Four repSet weight

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

        loop [] sortedPlatePairs (weightRoundedUp - bar.Weight)
