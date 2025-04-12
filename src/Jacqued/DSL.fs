module Jacqued.DSL

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Jacqued
open Jacqued.Design
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
                    View.withAttrs
                        [ TextBlock.padding padding
                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                        (Typography.button text)
            | _ -> ()
        ]
    ]

[<AutoOpen>]
module MaterialButton =
    open Jacqued.Controls

    let create (attrs: IAttr<MaterialButton> list) : IView<MaterialButton> =

        ViewBuilder.Create<MaterialButton>(
            [ attrs
              [ MaterialButton.padding 0
                MaterialButton.cornerRadius 20

                ] ]
            |> List.concat
        )

    type MaterialButton with
        static member content<'t when 't :> MaterialButton>(text: string, ?iconKind: MaterialIconKind) : IAttr<'t> =
            MaterialButton.content (buttonContent (text |> Some) iconKind Orientation.Horizontal)

        static member content<'t when 't :> MaterialButton>(iconKind: MaterialIconKind) : IAttr<'t> =
            MaterialButton.content (buttonContent None (iconKind |> Some) Orientation.Horizontal)

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
module FlatButton =
    open Jacqued.Controls

    let create (attrs: IAttr<FlatButton> list) : IView<FlatButton> =
        ViewBuilder.Create<FlatButton>(
            [ attrs; [ FlatButton.padding 0; FlatButton.theme Theme.Controls.flatButton ] ]
            |> List.concat
        )

    type FlatButton with
        static member content<'t when 't :> FlatButton>(text: string, ?iconKind: MaterialIconKind) : IAttr<'t> =
            FlatButton.content (buttonContent (text |> Some) iconKind Orientation.Horizontal)

        static member content<'t when 't :> FlatButton>(iconKind: MaterialIconKind) : IAttr<'t> =
            FlatButton.content (buttonContent None (iconKind |> Some) Orientation.Horizontal)

[<AutoOpen>]
module TopAppBar =
    open Jacqued.Controls

    let create (attrs: IAttr<TopAppBar> list) : IView<TopAppBar> =
        let title =
            attrs
            |> List.tryPick (fun attr ->
                match attr.Property with
                | ValueSome property ->
                    match property.Accessor with
                    | Accessor.AvaloniaProperty avaloniaProperty when avaloniaProperty = TopAppBar.TitleProperty ->
                        (property.Value |> string) |> Some
                    | _ -> None
                | ValueNone -> None)

        let trailing =
            attrs
            |> List.choose (fun attr ->
                match attr.Content with
                | ValueSome property ->
                    match property.Accessor with
                    | Accessor.InstanceProperty ip when ip.Name = nameof Unchecked.defaultof<TopAppBar>.Trailing ->
                        property.Content |> Some
                    | _ -> None
                | _ -> None)

        ViewBuilder.Create<TopAppBar>(
            [ [ TopAppBar.horizontalAlignment HorizontalAlignment.Stretch
                TopAppBar.verticalAlignment VerticalAlignment.Top
                TopAppBar.focusable false

                TopAppBar.content (
                    DockPanel.create [
                        DockPanel.children [
                            yield!
                                trailing
                                |> List.map (fun content ->
                                    match content with
                                    | ViewContent.Single s ->
                                        seq {
                                            if s.IsSome then
                                                yield s.Value
                                        }
                                    | ViewContent.Multiple m -> m)
                                |> Seq.concat

                            yield
                                TextBlock.create [
                                    DockPanel.dock Dock.Left
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.fontSize 20
                                    match title with
                                    | Some title -> TextBlock.text title
                                    | None -> ()
                                ]
                        ]
                    ]

                ) ]
              attrs ]
            |> List.concat
        )

    type TopAppBar with
        static member title<'t when 't :> TopAppBar>(text: string) : IAttr<'t> =
            AttrBuilder.CreateProperty<string>(TopAppBar.TitleProperty, text, ValueNone)

        static member trailing<'t, 'tbutton when 't :> TopAppBar and 'tbutton :> Button>(views: IView<'tbutton> list) : IAttr<'t> =
            let getter: ('t -> obj) = (fun control -> control.Trailing :> obj)

            AttrBuilder.CreateContentMultiple(
                nameof Unchecked.defaultof<'t>.Trailing,
                ValueSome getter,
                ValueNone,
                (views
                 |> List.map (fun view -> ViewBuilder.Create<'tbutton>([ view.Attrs; [ 'tbutton.margin (0, 12) ] ] |> List.concat) :> IView))
            )

[<AutoOpen>]
module Separator =
    let create (attrs: IAttr<Separator> list) : IView<Separator> = ViewBuilder.Create<Separator>(attrs)

    type Separator() = class end

[<AutoOpen>]
module CartesianChart =
    open LiveChartsCore
    open LiveChartsCore.Kernel.Events
    open LiveChartsCore.Kernel.Sketches
    open LiveChartsCore.Measure
    open LiveChartsCore.Painting
    open LiveChartsCore.SkiaSharpView.Avalonia

    let create (attrs: IAttr<CartesianChart> list) : IView<CartesianChart> =
        ViewBuilder.Create<CartesianChart>(attrs)

    type CartesianChart with
        static member onVisualElementsPointerDown<'t when 't :> CartesianChart>(func: VisualElementsEventArgs -> unit, ?subPatchOptions) =
            let name = nameof Unchecked.defaultof<'t>.add_VisualElementsPointerDown

            let factory: SubscriptionFactory<VisualElementsEventArgs> =
                fun (control, func, token) ->
                    let control = control :?> 't
                    let handler = VisualElementsHandler(fun _ -> func)

                    control.add_VisualElementsPointerDown (handler)

                    token.Register(fun _ -> control.remove_VisualElementsPointerDown (handler))
                    |> ignore

            AttrBuilder<'t>
                .CreateSubscription<VisualElementsEventArgs>(name, factory, func, ?subPatchOptions = subPatchOptions)

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

        static member legend<'t when 't :> CartesianChart>(value: IChartLegend) : IAttr<'t> =
            let name = nameof Unchecked.defaultof<'t>.Legend
            let getter: 't -> IChartLegend = (_.Legend)

            let setter: 't * IChartLegend -> unit =
                (fun (control, value) -> control.Legend <- value)

            AttrBuilder<'t>
                .CreateProperty<IChartLegend>(name, value, ValueSome getter, ValueSome setter, ValueNone)

        static member tooltip<'t when 't :> CartesianChart>(value: IChartTooltip) : IAttr<'t> =
            let name = nameof Unchecked.defaultof<'t>.Tooltip
            let getter: 't -> IChartTooltip = (_.Tooltip)

            let setter: 't * IChartTooltip -> unit =
                (fun (control, value) -> control.Tooltip <- value)

            AttrBuilder<'t>
                .CreateProperty<IChartTooltip>(name, value, ValueSome getter, ValueSome setter, ValueNone)

        static member zoomMode<'t when 't :> CartesianChart>(value: ZoomAndPanMode) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<ZoomAndPanMode>(property = CartesianChart.ZoomModeProperty, value = value, comparer = ValueNone)

        static member legendTextPaint<'t when 't :> CartesianChart>(value: Paint) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<Paint>(property = CartesianChart.LegendTextPaintProperty, value = value, comparer = ValueNone)

        static member legendBackgroundPaint<'t when 't :> CartesianChart>(value: Paint) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<Paint>(property = CartesianChart.LegendBackgroundPaintProperty, value = value, comparer = ValueNone)

        static member tooltipTextPaint<'t when 't :> CartesianChart>(value: Paint) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<Paint>(property = CartesianChart.TooltipTextPaintProperty, value = value, comparer = ValueNone)

        static member tooltipBackgroundPaint<'t when 't :> CartesianChart>(value: Paint) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<Paint>(property = CartesianChart.TooltipBackgroundPaintProperty, value = value, comparer = ValueNone)

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

[<AutoOpen>]
module SingleActionDialog =
    open AvaloniaDialogs.Views

    let create (attrs: IAttr<SingleActionDialog> list) : IView<SingleActionDialog> =
        ViewBuilder.Create<SingleActionDialog>(attrs)

    type SingleActionDialog with
        static member message<'t when 't :> SingleActionDialog>(value: string) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<string>(property = SingleActionDialog.MessageProperty, value = value, comparer = ValueNone)

        static member buttonText<'t when 't :> SingleActionDialog>(value: string) : IAttr<'t> =
            AttrBuilder<'t>
                .CreateProperty<string>(property = SingleActionDialog.ButtonTextProperty, value = value, comparer = ValueNone)
