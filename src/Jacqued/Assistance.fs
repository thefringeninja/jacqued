namespace Jacqued

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Jacqued
open Jacqued.Helpers

module Assistance =
    type BoringButBig =
        | UpDown
        | Ascending
        | Descending

    let private subTypes =
        [ (UpDown, [ 0.5; 0.6; 0.7; 0.6; 5 ])
          (Ascending, [ 0.3; 0.4; 0.5; 0.6; 0.7 ])
          (Descending, [ 0.7; 0.6; 0.5; 0.4; 0.5 ]) ]
        |> Map.ofList

    let boringButBig subType exercise (bar: Bar) platePairs colorMap units (trainingMax: Weight) =
        let calculateWeight (repSet: int) =
            trainingMax * (subTypes |> Map.find subType |> List.item repSet)

        let calculatePlates = Calculate.plates bar platePairs

        let control i (platePairs: PlatePair list) =
            let set = i + 1
            let weight = bar.Weight + (platePairs |> List.map (_.Weight) |> List.sum)

            StackPanel.create [
                StackPanel.children [
                    TextBlock.create [ TextBlock.classes [ "Subtitle1" ]; TextBlock.text $"Set {set}" ]
                    TextBlock.create [ TextBlock.classes [ "Subtitle2" ]; TextBlock.text $"Weight: {weight}{units}" ]
                    TextBlock.create [ TextBlock.classes [ "Subtitle2" ]; TextBlock.text "Reps: 10" ]
                    PlatePairs.control (units, colorMap, platePairs)
                ]
            ]

        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                yield TextBlock.create [ TextBlock.classes [ "Headline6" ]; TextBlock.text $"{exercise}" ]
                yield!
                    [ 0..1..4 ]
                    |> List.map calculateWeight
                    |> List.map calculatePlates
                    |> List.mapi control
                    |> List.map generalize
                    |> divide
            ]
        ]