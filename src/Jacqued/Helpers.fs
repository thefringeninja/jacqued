module Jacqued.Helpers

open System
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Jacqued.Controls
open Jacqued.Design

let private scrollbarVisibility =
    if OperatingSystem.IsAndroid() || OperatingSystem.IsIOS() then
        ScrollBarVisibility.Hidden
    else
        ScrollBarVisibility.Auto

let layout (content:IView) =
    ScrollViewer.create [
        ScrollViewer.horizontalScrollBarVisibility ScrollBarVisibility.Disabled
        ScrollViewer.verticalScrollBarVisibility scrollbarVisibility
        ScrollViewer.content content
    ]

let buttonBar (buttons: IView<MaterialButton> list) : IView =
    let mapButton i button : IView =
        let attrs: IAttr<MaterialButton> list =
            [ yield MaterialButton.dock (if i = 0 then Dock.Right else Dock.Left)

              if i > 0 then
                  yield MaterialButton.theme Theme.Controls.outlineButton
              else
                  yield MaterialButton.theme Theme.Controls.button ]

        View.withAttrs attrs button

    DockPanel.create [
        DockPanel.lastChildFill false
        DockPanel.children (buttons |> List.mapi mapButton)
    ]

let segmentedButtonBar (buttons: IView<MaterialButton> list) =
    let mapButton i button : IView =
        let attrs: IAttr<MaterialButton> list =
            [ yield MaterialButton.theme Theme.Controls.outlineButton

              if i = 0 then
                  yield MaterialButton.cornerRadius (16, 0, 0, 16)

              if i = buttons.Length - 1 then
                  yield MaterialButton.cornerRadius (0, 16, 16, 0) ]

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
