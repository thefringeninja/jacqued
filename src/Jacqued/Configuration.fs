module Jacqued.Configuration

open System.IO
open Jacqued
open Jacqued.Resources

let load path =
    let fileInfo = FileInfo(path)

    let settings =
        if fileInfo.Exists then
            try
                use reader = fileInfo.OpenText()
                Settings.load reader
            with _ ->
                Settings.zero
        else
            Settings.zero

    { settings with
        SettingsPath = path |> Some }

let save (settings: Settings) =
    match settings.SettingsPath with
    | Some path ->
        let fileInfo = FileInfo(path)

        try
            use writer = fileInfo.CreateText()
            settings.save writer
        with _ ->
            ()
    | _ -> ()

let update msg (state: Settings) =
    match msg with
    | Msg.SelectedThemeChanged theme ->
        Theme.set theme

        let settings = { state with ThemeVariant = theme }

        save settings

        settings, List.empty |> Ok
    | Msg.ConfigurationSettingsLoaded settings ->
        Theme.set settings.ThemeVariant

        settings, List.empty |> Ok
    | _ -> state, List.empty |> Ok
