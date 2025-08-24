namespace Jacqued

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Jacqued.DSL
open Jacqued.Design
open Material.Icons
open Material.Icons.Avalonia

type PlatePairs() =
    static member control(units, platePairs, ?func: Weight -> unit, ?subPatchOptions: SubPatchOptions) =
        let plateBrush = Theme.Brushes.secondaryMid

        platePairs
        |> List.fold
            (fun acc (plate: PlatePair) ->
                acc
                |> Map.change plate (function
                    | None -> Some 1
                    | Some count -> Some(count + 1)))
            Map.empty
        |> Map.toList
        |> List.map (
            (fun (platePair, count) ->
                let children =
                    [ let rightMargin =
                          function
                          | Some _ -> 0
                          | _ -> 16

                      yield
                          View.withAttrs
                              [ TextBlock.margin (12, 0, rightMargin func, 0)
                                TextBlock.verticalAlignment VerticalAlignment.Center ]
                              (Typography.platePairs (platePair.WeightOfEach, units, count))
                          |> generalize

                      if func.IsSome then
                          yield
                              ContentControl.create [
                                  ContentControl.content (MaterialIcon.create [ MaterialIcon.kind MaterialIconKind.Close ])
                                  ContentControl.margin (8, 0)
                                  ContentControl.onTapped (
                                      (fun _ -> func.Value platePair.WeightOfEach),
                                      ?subPatchOptions = subPatchOptions
                                  )
                              ] ]

                Border.create [
                    Border.cornerRadius 8
                    Border.height 32
                    Border.margin 8
                    Border.borderBrush plateBrush
                    Border.borderThickness 1
                    Border.child (StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children children ])
                ])
            >> generalize
        )
