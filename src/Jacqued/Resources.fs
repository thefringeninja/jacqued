module Jacqued.Resources

open System
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

module Themes =


    let private theme name =
        let uc (s: string) =
            s
            |> Seq.mapi (fun i c -> if i = 0 then Char.ToUpper c else c)
            |> Seq.map string
            |> String.concat ""

        let name = name |> uc

        lazy
            (match Application.Current.TryFindResource name with
             | true, theme -> theme :?> ControlTheme
             | _ -> failwith $"Could not find theme '{name}'")

    let rec materialOutlineButton = theme (nameof materialOutlineButton)
