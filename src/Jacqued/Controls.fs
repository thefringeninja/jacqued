namespace Jacqued.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Templates
open Avalonia.Data
open Avalonia.Layout
open Avalonia.Markup.Xaml.Templates
open Avalonia.Styling
open Jacqued.Design

[<AutoOpen>]
module AvaloniaObject =
    type AvaloniaObject with
        member a.Properties
            with set (values: (AvaloniaProperty * obj) seq) =
                for p, v in values do
                    a.SetValue(p, v) |> ignore

[<AutoOpen>]
module Panel =
    type Panel with
        member c.Children
            with set (values: Control seq) = values |> c.Children.AddRange

[<AutoOpen>]
module StyledElement =
    type StyledElement with
        member s.Styles
            with set (values: IStyle seq) = values |> s.Styles.AddRange

[<AutoOpen>]
module Style =
    type Style with
        member s.Setters
            with set (values: SetterBase seq) =
                for value in values do
                    s.Add(value)

type NavigationButton() =
    inherit ContentControl()

type MaterialButton() =
    inherit Button()

type FlatButton() =
    inherit Button()

type TopAppBar() =
    inherit ContentControl()
    // ContentControl(
    //     Styles =
    //         [ Style(
    //               Setters =
    //                   [ Setter(TopAppBar.BackgroundProperty, Theme.Brushes.primaryMid)
    //                     Setter(TopAppBar.ForegroundProperty, Theme.Brushes.primaryMidForeground)
    //                     Setter(TopAppBar.HorizontalAlignmentProperty, HorizontalAlignment.Stretch)
    //                     Setter(TopAppBar.VerticalAlignmentProperty, VerticalAlignment.Top)
    //                     Setter(TopAppBar.FocusableProperty, false)
    //
    //                     let g =
    //                         Grid(
    //                             HorizontalAlignment = HorizontalAlignment.Stretch,
    //                             VerticalAlignment = VerticalAlignment.Stretch,
    //                             RowDefinitions = RowDefinitions("Auto,100"),
    //                             Properties = [ (TextBlock.ForegroundProperty, Theme.Brushes.primaryMid) ],
    //                             Children =
    //                                 [ Border(
    //                                       HorizontalAlignment = HorizontalAlignment.Stretch,
    //                                       VerticalAlignment = VerticalAlignment.Top
    //                                   )
    //                                   Grid()
    //                                   Border(
    //                                       HorizontalAlignment = HorizontalAlignment.Stretch,
    //                                       VerticalAlignment = VerticalAlignment.Stretch,
    //                                       Focusable = false,
    //                                       Properties = [ Grid.RowProperty, 1 ],
    //                                       Child =
    //                                           ContentControl(
    //                                               Margin = Thickness(80, 0, 16, 0),
    //                                               HorizontalAlignment = HorizontalAlignment.Left,
    //                                               VerticalAlignment = VerticalAlignment.Center,
    //                                               Focusable = false,
    //                                               Content = new TemplateBinding(TopAppBar.TitleProperty),
    //                                               Properties =
    //                                                   [ Grid.ColumnProperty, 1
    //                                                     Grid.ColumnSpanProperty, 2
    //                                                     TextBlock.FontSizeProperty, 20 ]
    //                                           )
    //                                   ) ]
    //                         )
    //
    //                     Setter(
    //                         TopAppBar.TemplateProperty,
    //                         FuncControlTemplate(System.Func<TemplatedControl, INameScope, Control>(fun _ _ -> g))
    //                     ) ]
    //           )
    //
    //           ]
    // )

    let trailing = Controls()

    static let titleProperty =
        AvaloniaProperty.Register<TopAppBar, string>(nameof Unchecked.defaultof<TopAppBar>.Title, "", true)

    static member TitleProperty = titleProperty

    member this.Title
        with get () = this.GetValue(TopAppBar.TitleProperty)
        and set value = this.SetValue<string>(TopAppBar.TitleProperty, value) |> ignore

    member this.Trailing = trailing
//and set value = this.SetValue<Controls>(TopAppBar.TrailingProperty, value) |> ignore
