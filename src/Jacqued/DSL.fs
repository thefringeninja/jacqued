module Jacqued.DSL

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open LiveChartsCore
open LiveChartsCore.Kernel.Sketches
open LiveChartsCore.Measure
open LiveChartsCore.SkiaSharpView.Avalonia
open LiveChartsCore.SkiaSharpView.Drawing
open Material.Icons
open Material.Icons.Avalonia
open Material.Styles.Assists

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
    let create (attrs: IAttr<MaterialIcon> list) : IView<MaterialIcon> = ViewBuilder.Create<MaterialIcon>(attrs)

    type MaterialIcon with

        static member kind<'t when 't :> MaterialIcon>(value: MaterialIconKind) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<MaterialIconKind>(MaterialIcon.KindProperty, value, ValueNone)

let private buttonContent (text: string option) (iconKind: MaterialIconKind option) (orientation: Orientation) =
    let padding =
        match text, iconKind, orientation with
        | Some _, Some _, Orientation.Horizontal -> Thickness(16, 0, 24, 0)
        | _, _, Orientation.Horizontal -> Thickness(24, 0, 24, 0)
        | _ -> Thickness(0, 12, 0, 16)

    let height, iconSize, viewboxSize =
        match orientation with
        | Orientation.Horizontal -> 40 |> Some, 16, 16
        | _ -> None, 24, 32

    StackPanel.create [
        StackPanel.orientation orientation
        StackPanel.margin padding
        if height.IsSome then
            StackPanel.height height.Value
        StackPanel.children [
            match iconKind with
            | Some iconKind ->
                yield
                    Viewbox.create [
                        Viewbox.stretch Stretch.None
                        Viewbox.width viewboxSize
                        Viewbox.height viewboxSize
                        Viewbox.horizontalAlignment HorizontalAlignment.Stretch
                        Viewbox.verticalAlignment VerticalAlignment.Stretch
                        Viewbox.child (
                            MaterialIcon.create [
                                MaterialIcon.kind iconKind
                                MaterialIcon.width iconSize
                                MaterialIcon.height iconSize
                            ]
                        )
                    ]
                    |> generalize
            | _ -> ()

            match text with
            | Some text ->
                let padding =
                    match iconKind, orientation with
                    | Some _, Orientation.Vertical -> Thickness(0, 4, 0, 0)
                    | Some _, Orientation.Horizontal -> Thickness(8, 0, 0, 0)
                    | _ -> Thickness(0)

                yield
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.padding padding
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.classes [ "Subtitle2" ]
                    ]
            | _ -> ()
        ]
    ]

[<AutoOpen>]
module MaterialButton =
    let create (attrs: IAttr<Button> list) : IView<Button> =
        ViewBuilder.Create<Button>([ attrs; [ Button.padding 0; Button.cornerRadius 20 ] ] |> List.concat)

    type Button with
        static member content<'t when 't :> Button>(text: string, iconKind: MaterialIconKind option) : IAttr<'t> =
            Button.content (buttonContent (text |> Some) iconKind Orientation.Horizontal)

[<AutoOpen>]
module NavigationButton =
    open Jacqued.Controls

    let create (attrs: IAttr<NavigationButton> list) : IView<NavigationButton> =
        ViewBuilder.Create<NavigationButton>(attrs)

    type NavigationButton with
        static member content<'t when 't :> NavigationButton>(iconKind: MaterialIconKind, ?text: string) : IAttr<'t> =
            NavigationButton.content (buttonContent text (iconKind |> Some) Orientation.Vertical)

[<AutoOpen>]
module FloatingButton =
    open Material.Styles.Controls

    let create (attrs: IAttr<FloatingButton> list) : IView<FloatingButton> =
        ViewBuilder.Create<FloatingButton>(attrs)

    type FloatingButton with
        static member isExtended<'t when 't :> FloatingButton>(value: bool) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<bool>(FloatingButton.IsExtendedProperty, value, ValueNone)

        static member content<'t when 't :> FloatingButton>(iconKind: MaterialIconKind, ?text: string) : IAttr<'t> =
            FloatingButton.content (buttonContent text (iconKind |> Some) Orientation.Horizontal)

[<AutoOpen>]
module Separator =
    let create (attrs: IAttr<Separator> list) : IView<Separator> = ViewBuilder.Create<Separator>(attrs)

    type Separator with
        end

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

        static member legend<'t when 't :> CartesianChart>(value: IChartLegend<SkiaSharpDrawingContext>) : IAttr<'t> =
            let name = nameof Unchecked.defaultof<'t>.Legend
            let getter: 't -> IChartLegend<SkiaSharpDrawingContext> = (_.Legend)

            let setter: 't * IChartLegend<SkiaSharpDrawingContext> -> unit =
                (fun (control, value) -> control.Legend <- value)

            AttrBuilder<'t>
                .CreateProperty<IChartLegend<SkiaSharpDrawingContext>>(name, value, ValueSome getter, ValueSome setter, ValueNone)

[<AutoOpen>]
module ReactiveDialogHost =
    open AvaloniaDialogs.Views

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
