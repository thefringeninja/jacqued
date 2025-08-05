module Jacqued.Design

open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Styling
open Material.Colors
open Material.Colors.Recommended

type Shade =
    | ``50`` = 0
    | ``100`` = 1
    | ``200`` = 2
    | ``300`` = 3
    | ``400`` = 4
    | ``500`` = 5
    | ``600`` = 6
    | ``700`` = 7
    | ``800`` = 8
    | ``900`` = 9
    | ``A100`` = 10
    | ``A200`` = 11
    | ``A400`` = 12
    | ``A700`` = 13

let private mapShade (index, color) =
    let shade =
        if index < MaterialColor.Brown50 then
            ((index |> int32) % 14) |> enum<Shade>
        else
            (((index - MaterialColor.Brown50) |> int32) % 10) |> enum<Shade>

    shade, color

let palette =
    [ PurpleSwatch() :> ISwatch
      IndigoSwatch()
      LightBlueSwatch()
      TealSwatch()
      GreenSwatch()
      DeepOrangeSwatch()
      CyanSwatch() ]
    |> List.map (fun swatch -> swatch.Lookup |> Seq.map ((|KeyValue|) >> mapShade) |> Map.ofSeq)

module Theme =
    let set theme =
        let app = Application.Current

        if isNull app then
            ()
        else
            app.RequestedThemeVariant <- theme

    let get () =
        let app = Application.Current

        if isNull app then
            ThemeVariant.Default
        else
            app.ActualThemeVariant

    module Brushes =
        let private resource name =
            let name = name |> Util.pascalize

            lazy
                (match Application.Current.TryFindResource $"Material{name}Brush" with
                 | true, theme -> theme :?> IBrush
                 | _ -> failwith $"Could not find theme '{name}'")

        let rec primaryLightForeground = (resource (nameof primaryLightForeground)).Value
        let rec primaryMidForeground = (resource (nameof primaryMidForeground)).Value
        let rec primaryForeground = (resource (nameof primaryForeground)).Value
        let rec primaryLight = (resource (nameof primaryLight)).Value
        let rec primaryMid = (resource (nameof primaryMid)).Value
        let rec primaryDark = (resource (nameof primaryDark)).Value
        let rec secondaryLightForeground = (resource (nameof secondaryLightForeground)).Value
        let rec secondaryMidForeground = (resource (nameof secondaryMidForeground)).Value
        let rec secondaryDarkForeground = (resource (nameof secondaryDarkForeground)).Value
        let rec secondaryLight = (resource (nameof secondaryLight)).Value
        let rec secondaryMid = (resource (nameof secondaryMid)).Value
        let rec secondaryDark = (resource (nameof secondaryDark)).Value

    module Controls =
        let private resource name =
            let name = name |> Util.pascalize

            lazy
                (match Application.Current.TryFindResource $"Material{name}" with
                 | true, theme -> theme :?> ControlTheme
                 | _ -> failwith $"Could not find theme '{name}'")

        let rec button = (resource (nameof button)).Value
        let rec outlineButton = (resource (nameof outlineButton)).Value
        let rec flatButton = (resource (nameof flatButton)).Value
        let rec floatingButton = (resource (nameof floatingButton)).Value
