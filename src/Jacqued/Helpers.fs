module Jacqued.Helpers

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.FuncUI.Types
open Avalonia.Layout

let floatingLayout topAppBarButtons floatingButtons content =
    let margin5 button =
        button |> View.withAttrs [ Button.margin 5 ] |> generalize

    let flat button =
        button
        |> View.withAttrs [ Button.classes [ "Flat" ]; (Button.padding 0 :> IAttr<Button>) ]
        |> generalize

    Grid.create [
        Grid.rowDefinitions (RowDefinitions "Auto,*,Auto")
        Grid.children [
            DockPanel.create [
                DockPanel.lastChildFill false
                DockPanel.children [
                    StackPanel.create [
                        DockPanel.dock Dock.Right
                        StackPanel.height 24
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 24
                        StackPanel.margin (16, 20, 16, 24)
                        StackPanel.children (topAppBarButtons |> List.map flat)
                    ]
                ]
            ]
            ScrollViewer.create [ Grid.row 1; Grid.rowSpan 2; ScrollViewer.content (content |> generalize) ]
            DockPanel.create [
                Grid.row 2
                DockPanel.lastChildFill false
                DockPanel.children [
                    StackPanel.create [
                        DockPanel.dock Dock.Right
                        StackPanel.children (floatingButtons |> List.map margin5)
                    ]
                ]
            ]
        ]
    ]

let divide (views: IView list) : IView list =
    [ for i in 0 .. (views.Length - 1) do
          yield views |> List.item i

          if i < views.Length - 1 then
              yield Separator.create [] ]
