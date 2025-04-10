namespace Jacqued

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Styling
open Jacqued.DSL
open Jacqued.Design
open Material.Icons
open Material.Icons.Avalonia

type PlatePairs() =
    static member private colors = palette |> List.map (fun swatch -> swatch[Shade.``200``])

    static member private lightColors = palette |> List.map (fun swatch -> swatch[Shade.``400``])

    static member index(platePairs: PlatePair list) =
        platePairs |> List.distinctBy _.WeightOfEach |> List.sortBy _.WeightOfEach

    static member control
        (units, theme: ThemeVariant, platePairIndex: PlatePair list, platePairs, ?func: Weight -> unit, ?subPatchOptions: SubPatchOptions) =
        let colors, brush =
            match theme with
            | theme when theme = ThemeVariant.Dark -> PlatePairs.colors, "MaterialLightForegroundBrush"
            | _ -> PlatePairs.lightColors, "MaterialLightForegroundBrush"

        let foreground =
            match Application.Current.FindResource brush with
            | :? IBrush as brush -> brush
            | _ -> null

        platePairs
        |> List.fold
            (fun acc (plate: PlatePair) ->
                acc
                |> Map.change plate (function
                    | None -> Some 1
                    | Some count -> Some(count + 1)))
            Map.empty
        |> Map.toList
        |> List.map (fun (platePair, count) ->
            let i = platePairIndex |> List.findIndex (fun w -> w = platePair)
            let background = colors[i % colors.Length]

            let children =
                [ let rightMargin =
                      function
                      | Some _ -> 0
                      | _ -> 16

                  yield
                      View.withAttrs
                          [ TextBlock.margin (16, 0, rightMargin func, 0)
                            TextBlock.foreground foreground
                            TextBlock.verticalAlignment VerticalAlignment.Center ]
                          (Typography.body2 $"{platePair.WeightOfEach} {units} (x{count})")
                      |> generalize

                  if func.IsSome then
                      yield
                          ContentControl.create [
                              ContentControl.content (MaterialIcon.create [ MaterialIcon.kind MaterialIconKind.Close ])
                              ContentControl.foreground foreground
                              ContentControl.margin (8, 0)
                              ContentControl.onTapped ((fun _ -> func.Value platePair.WeightOfEach), ?subPatchOptions = subPatchOptions)
                          ] ]

            Border.create [
                Border.cornerRadius 8
                Border.height 32
                Border.margin 8
                Border.background background
                Border.child (StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children children ])
            ])

        |> List.map generalize
