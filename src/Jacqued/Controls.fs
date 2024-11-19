module Jacqued.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open LiveChartsCore
open LiveChartsCore.Kernel.Sketches
open LiveChartsCore.Measure
open LiveChartsCore.SkiaSharpView.Avalonia
open Material.Styles.Assists

let floatingLayout buttons content =
    let margin5 button =
        button |> View.withAttrs [ Button.margin 5 ] |> generalize

    let buttons = buttons |> List.map margin5

    Grid.create
        [ Grid.rowDefinitions (RowDefinitions "*,Auto")
          Grid.children
              [ Panel.create [ Grid.row 0; Grid.rowSpan 2; Panel.children [ content ] ]
                DockPanel.create
                    [ Grid.row 1
                      DockPanel.lastChildFill false
                      DockPanel.children [ StackPanel.create [ DockPanel.dock Dock.Right; StackPanel.children buttons ] ] ] ] ]

type NavigationButton() =
    inherit ContentControl()

[<AutoOpen>]
module ComboBox =
    type ComboBox with
        static member label<'t when 't :> ComboBox>(value: string) =
            AttrBuilder<ComboBox>
                .CreateProperty<string>(ComboBoxAssist.LabelProperty, value, ValueNone)

[<AutoOpen>]
module TextBox =
    type TextBox with
        static member label<'t when 't :> TextBox>(value: string) =
            AttrBuilder<TextBox>
                .CreateProperty<string>(TextFieldAssist.LabelProperty, value, ValueNone)

[<AutoOpen>]
module MaterialIcon =
    open Material.Icons
    open Material.Icons.Avalonia

    let create (attrs: IAttr<MaterialIcon> list) : IView<MaterialIcon> = ViewBuilder.Create<MaterialIcon>(attrs)

    type MaterialIcon with

        static member kind<'t when 't :> MaterialIcon>(value: MaterialIconKind) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<MaterialIconKind>(MaterialIcon.KindProperty, value, ValueNone)

[<AutoOpen>]
module NavigationButton =
    open Material.Icons
    open Material.Icons.Avalonia

    let create (attrs: IAttr<NavigationButton> list) : IView<NavigationButton> =
        ViewBuilder.Create<NavigationButton>(attrs)

    type NavigationButton with
        static member content<'t when 't :> NavigationButton>(iconKind: MaterialIconKind, ?text: string) : IAttr<'t> =
            NavigationButton.content (
                StackPanel.create
                    [ StackPanel.orientation Orientation.Vertical
                      StackPanel.margin (0, 12, 0, 16)
                      StackPanel.children
                          [ yield
                                Viewbox.create
                                    [ Viewbox.stretch Stretch.Fill
                                      Viewbox.width 24
                                      Viewbox.horizontalAlignment HorizontalAlignment.Stretch
                                      Viewbox.verticalAlignment VerticalAlignment.Stretch
                                      Viewbox.child (
                                          MaterialIcon.create [ MaterialIcon.kind iconKind; MaterialIcon.width 24; MaterialIcon.height 24 ]
                                      ) ]
                                |> generalize

                            match text with
                            | Some text ->
                                yield
                                    TextBlock.create
                                        [ TextBlock.text text
                                          TextBlock.verticalAlignment VerticalAlignment.Center
                                          TextBlock.classes [ "Subtitle2" ] ]
                            | _ -> () ] ]
            )

[<AutoOpen>]
module FloatingButton =
    open Material.Icons
    open Material.Styles.Controls
    open Material.Icons.Avalonia

    let create (attrs: IAttr<FloatingButton> list) : IView<FloatingButton> =
        ViewBuilder.Create<FloatingButton>(attrs)

    type FloatingButton with
        static member isExtended<'t when 't :> FloatingButton>(value: bool) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<bool>(FloatingButton.IsExtendedProperty, value, ValueNone)

        static member content<'t when 't :> FloatingButton>(iconKind: MaterialIconKind, ?text: string) : IAttr<'t> =
            FloatingButton.content (
                StackPanel.create
                    [ StackPanel.orientation Orientation.Horizontal
                      StackPanel.height 24
                      StackPanel.children
                          [ yield
                                Viewbox.create
                                    [ Viewbox.stretch Stretch.Fill
                                      Viewbox.horizontalAlignment HorizontalAlignment.Stretch
                                      Viewbox.verticalAlignment VerticalAlignment.Stretch
                                      Viewbox.child (
                                          MaterialIcon.create [ MaterialIcon.kind iconKind; MaterialIcon.width 24; MaterialIcon.height 24 ]
                                      ) ]
                                |> generalize

                            match text with
                            | Some text ->
                                yield
                                    TextBlock.create
                                        [ TextBlock.text text
                                          TextBlock.verticalAlignment VerticalAlignment.Center
                                          TextBlock.classes [ "Subtitle2" ] ]
                            | _ -> () ] ]
            )

[<AutoOpen>]
module CartesianChart =

    let create (attrs: IAttr<CartesianChart> list) : IView<CartesianChart> =
        ViewBuilder.Create<CartesianChart>(attrs)

    type CartesianChart with
        static member xaxes<'t when 't :> CartesianChart>(value: seq<ICartesianAxis>) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<seq<ICartesianAxis>>(property = CartesianChart.XAxesProperty, value = value, comparer = ValueNone)

        static member yaxes<'t when 't :> CartesianChart>(value: seq<ICartesianAxis>) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<seq<ICartesianAxis>>(property = CartesianChart.YAxesProperty, value = value, comparer = ValueNone)

        static member legendPosition<'t when 't :> CartesianChart>(value: LegendPosition) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<LegendPosition>(property = CartesianChart.LegendPositionProperty, value = value, comparer = ValueNone)

        static member series<'t when 't :> CartesianChart>(value: seq<ISeries>) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<seq<ISeries>>(property = CartesianChart.SeriesProperty, value = value, comparer = ValueNone)

[<AutoOpen>]
module ReactiveDialogHost =
    open AvaloniaDialogs.Views
    open Avalonia.Input

    let create (attrs: IAttr<ReactiveDialogHost> list) : IView<ReactiveDialogHost> =
        ViewBuilder.Create<ReactiveDialogHost>(attrs)

    type ReactiveDialogHost with
        static member closeKey<'t when 't :> ReactiveDialogHost>(value: Key) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<Key>(property = ReactiveDialogHost.CloseKeyProperty, value = value, comparer = ValueNone)

[<AutoOpen>]
module TwofoldDialog =
    open AvaloniaDialogs.Views

    let create (attrs: IAttr<TwofoldDialog> list) : IView<TwofoldDialog> =
        ViewBuilder.Create<TwofoldDialog>(attrs)

    type TwofoldDialog with
        static member message<'t when 't :> TwofoldDialog>(value: string) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<string>(property = TwofoldDialog.MessageProperty, value = value, comparer = ValueNone)

        static member positiveText<'t when 't :> TwofoldDialog>(value: string) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<string>(property = TwofoldDialog.PositiveTextProperty, value = value, comparer = ValueNone)

        static member negativeText<'t when 't :> TwofoldDialog>(value: string) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<string>(property = TwofoldDialog.NegativeTextProperty, value = value, comparer = ValueNone)
