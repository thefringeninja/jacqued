module Jacqued.Design

open Avalonia
open Avalonia.Controls
open Avalonia.Platform
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

let palette =
    [ PurpleSwatch() :> ISwatch
      IndigoSwatch()
      LightBlueSwatch()
      TealSwatch()
      GreenSwatch()
      DeepOrangeSwatch()
      CyanSwatch() ]
    |> List.map (fun swatch ->
        swatch.Lookup
        |> Seq.map (|KeyValue|)
        |> Seq.map (fun (index, color) ->
            let shade =
                if index < MaterialColor.Brown50 then
                    ((index |> int32) % 14) |> enum<Shade>
                else
                    (((index - MaterialColor.Brown50) |> int32) % 10) |> enum<Shade>

            shade, color)
        |> Map.ofSeq)

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

    let private theme name =
        let name = name |> Util.pascalize

        lazy
            (match Application.Current.TryFindResource name with
             | true, theme -> theme :?> ControlTheme
             | _ -> failwith $"Could not find theme '{name}'")

    let rec materialButton = (theme (nameof materialButton)).Value
    let rec materialOutlineButton = (theme (nameof materialOutlineButton)).Value
    let rec materialFlatButton = (theme (nameof materialFlatButton)).Value
    let rec materialFloatingButton = (theme (nameof materialFloatingButton)).Value
