module Jacqued.Resources

open Avalonia
open Avalonia.Controls
open Avalonia.Styling
open Material.Colors.Recommended

let swatches =
    [ PurpleSwatch.Purple400
      IndigoSwatch.Indigo400
      LightBlueSwatch.LightBlue400
      TealSwatch.Teal400
      GreenSwatch.Green400
      OrangeSwatch.Orange400 ]

module Theme =
    let set theme =
        let app = Application.Current
        if isNull app then
            ()
        else
            app.RequestedThemeVariant <- theme

    let private theme name =

        let name = name |> Util.pascalize

        lazy
            (match Application.Current.TryFindResource name with
             | true, theme -> theme :?> ControlTheme
             | _ -> failwith $"Could not find theme '{name}'")

    let rec materialOutlineButton = (theme (nameof materialOutlineButton)).Value
    let rec materialFlatButton = (theme (nameof materialFlatButton)).Value
