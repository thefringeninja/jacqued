module Jacqued.Dialogs

open Avalonia.Controls.Primitives
open Avalonia.Controls
open Avalonia.Layout

open Avalonia.FuncUI.DSL

let areYouSure (callback: bool -> unit) =
    Border.create [
        Border.verticalAlignment VerticalAlignment.Center
        Border.horizontalAlignment HorizontalAlignment.Center
        Border.background "white"
        Border.padding 5
        Border.cornerRadius 5
        Border.child (
            DockPanel.create [
                DockPanel.lastChildFill false
                DockPanel.children [
                    TextBlock.create [
                        TextBlock.dock Dock.Top
                        TextBlock.text "are you sure?"
                        TextBlock.fontSize 18.0
                        TextBlock.padding 20
                    ]

                    UniformGrid.create [
                        UniformGrid.dock Dock.Bottom
                        UniformGrid.columns 2
                        UniformGrid.rows 1
                        UniformGrid.children [

                            Button.create [
                                Button.content (
                                    TextBlock.create [
                                        TextBlock.text "yes"
                                        TextBlock.fontSize 18.0
                                        TextBlock.foreground "#e74c3c"
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                    ]
                                )
                                Button.onClick (fun _ -> callback true)
                            ]

                            Button.create [
                                Button.content (
                                    TextBlock.create [
                                        TextBlock.text "no"
                                        TextBlock.fontSize 18.0
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                    ]
                                )
                                Button.onClick (fun _ -> callback false)
                            ]
                        ]
                    ]

                ]
            ]
        )

    ]
