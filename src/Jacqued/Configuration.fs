module Jacqued.Configuration

open System.IO
open Jacqued
open Jacqued.Resources

let load path =
    let fileInfo = FileInfo(path)

    if fileInfo.Exists then
        try
            use reader = fileInfo.OpenText()
            Settings.load(reader, path)
        with _ ->
            Settings.zero
    else
        Settings.zero


let save path (settings: Settings) =
    let fileInfo = FileInfo(path)

    try
        use writer = fileInfo.CreateText()
        settings.save writer
    with _ ->
        ()



let update msg (state:Settings) =
    let saveSettings (settings: Settings) =
        match state.SettingsPath with
        | Some settingsPath ->
            let fileInfo = FileInfo(settingsPath)
            use writer = fileInfo.CreateText()
            settings.save writer
        | _ -> ()

    match msg with
    | Msg.SelectedThemeChanged theme ->
        Theme.set theme

        let settings =
            { state with
                ThemeVariant = theme }

        saveSettings settings
        
        settings, List.empty |> Ok
    | Msg.ConfigurationSettingsLoaded settings ->
        Theme.set settings.ThemeVariant

        settings, List.empty |> Ok
    | _ -> state, List.empty |> Ok
