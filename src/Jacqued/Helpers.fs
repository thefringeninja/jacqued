module Jacqued.Helpers

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
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

    let scrollbarVisibility =
        if OperatingSystem.IsAndroid() || OperatingSystem.IsIOS() then
            ScrollBarVisibility.Hidden
        else
            ScrollBarVisibility.Auto

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
            ScrollViewer.create [
                Grid.row 1
                Grid.rowSpan 2
                ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Disabled
                ScrollViewer.verticalScrollBarVisibility scrollbarVisibility
                ScrollViewer.content (content |> generalize)
            ]
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

let buttonBar (buttons: IView<Button> list) : IView =
    let mapButton i button : IView =
        let attrs: IAttr<Button> list =
            [ yield Button.dock (if i = 0 then Dock.Right else Dock.Left)

              if i > 0 then
                  let controlTheme = Resources.Themes.materialOutlineButton.Value
                  yield Button.theme controlTheme ]

        View.withAttrs attrs button

    DockPanel.create [
        DockPanel.lastChildFill false
        DockPanel.children (buttons |> List.mapi mapButton)
    ]

let segmentedButtonBar (buttons: IView<Button> list) =
    let mapButton i button : IView =
        let attrs: IAttr<Button> list =
            [ yield Button.theme Resources.Themes.materialOutlineButton.Value

              if i = 0 then
                  yield Button.cornerRadius (16, 0, 0, 16)

              if i = buttons.Length - 1 then
                  yield Button.cornerRadius (0, 16, 16, 0) ]

        View.withAttrs attrs button

    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.children (buttons |> List.mapi mapButton)
    ]

let divide (views: IView list) : IView list =
    [ for i in 0 .. (views.Length - 1) do
          yield views |> List.item i

          if i < views.Length - 1 then
              yield Separator.create [] ]
