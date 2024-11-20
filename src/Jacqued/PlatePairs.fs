namespace Jacqued

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers

open Avalonia.Layout
open Avalonia.Media
open Jacqued.Controls
open Material.Colors.Recommended
open Material.Icons
open Material.Icons.Avalonia

type PlatePairs() =
    static member colors =
        [ PurpleSwatch.Purple400
          IndigoSwatch.Indigo400
          LightBlueSwatch.LightBlue400
          TealSwatch.Teal400
          GreenSwatch.Green400
          OrangeSwatch.Orange400 ]

    static member colorMap(platePairs: PlatePair list) =
        platePairs
        |> List.distinctBy (_.WeightOfEach)
        |> List.sortBy (_.WeightOfEach)
        |> List.mapi (fun i plate -> (plate.WeightOfEach, PlatePairs.colors[i % PlatePairs.colors.Length]))
        |> Map.ofList

    static member control(colorMap: Map<Weight, Color>, platePairs, units, ?func: Weight -> unit, ?subPatchOptions: SubPatchOptions) =
        WrapPanel.create
            [ WrapPanel.orientation Orientation.Horizontal
              WrapPanel.children (
                  platePairs
                  |> List.fold
                      (fun acc (plate: PlatePair) ->
                          acc
                          |> Map.change plate.WeightOfEach (function
                              | None -> Some 1
                              | Some count -> Some(count + 1)))
                      Map.empty
                  |> Map.toList
                  |> List.sortBy fst
                  |> List.rev
                  |> List.map (fun (weight, count) ->
                      let children =
                          [ yield
                                TextBlock.create
                                    [ TextBlock.text $"{weight} {units} (x{count})"
                                      let rightMargin =
                                          function
                                          | Some _ -> 0
                                          | _ -> 16

                                      TextBlock.margin (16, 0, rightMargin func, 0)
                                      TextBlock.verticalAlignment VerticalAlignment.Center ]
                                |> generalize
                            if func.IsSome then
                                yield
                                    ContentControl.create
                                        [ ContentControl.content (MaterialIcon.create [ MaterialIcon.kind MaterialIconKind.Close ])
                                          ContentControl.margin (8, 0)
                                          ContentControl.onTapped ((fun _ -> func.Value weight), ?subPatchOptions = subPatchOptions) ] ]

                      Border.create
                          [ Border.cornerRadius 8
                            Border.height 32
                            Border.margin 8
                            Border.background colorMap[weight]
                            Border.child (
                                StackPanel.create [ StackPanel.orientation Orientation.Horizontal; StackPanel.children children ]
                            ) ])

                  |> List.map generalize
              ) ]
