module Jacqued.Helpers

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.FuncUI.Types
open Avalonia.Layout

let private scrollbarVisibility =
    if OperatingSystem.IsAndroid() || OperatingSystem.IsIOS() then
        ScrollBarVisibility.Hidden
    else
        ScrollBarVisibility.Auto

let layout content =
    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Disabled
        ScrollViewer.verticalScrollBarVisibility scrollbarVisibility
        ScrollViewer.content (content |> generalize)
    ]

let buttonBar (buttons: IView<Button> list) : IView =
    let mapButton i button : IView =
        let attrs: IAttr<Button> list =
            [ yield Button.dock (if i = 0 then Dock.Right else Dock.Left)

              if i > 0 then
                  let controlTheme = Resources.Theme.materialOutlineButton.Value
                  yield Button.theme controlTheme ]

        View.withAttrs attrs button

    DockPanel.create [
        DockPanel.lastChildFill false
        DockPanel.children (buttons |> List.mapi mapButton)
    ]

let segmentedButtonBar (buttons: IView<Button> list) =
    let mapButton i button : IView =
        let attrs: IAttr<Button> list =
            [ yield Button.theme Resources.Theme.materialOutlineButton.Value

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
