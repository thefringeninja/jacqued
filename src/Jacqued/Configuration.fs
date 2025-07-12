module Jacqued.Configuration

open System.IO
open Jacqued
open Jacqued.Design
open Jacqued.Util

let load (settingsFile: FileInfo) =
    let settings =
        if settingsFile.Exists then
            try
                use reader = settingsFile.OpenText()
                Settings.load reader
            with _ ->
                Settings.zero
        else
            Settings.zero

    { settings with
        SettingsPath = settingsFile.FullName |> Some }

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
    | Msg.SelectTheme theme ->
        Theme.set theme

        let settings = { state with ThemeVariant = theme }

        save settings

        settings |> pass
    | Msg.ConfigurationSettingsLoaded settings ->
        Theme.set settings.ThemeVariant

        settings |> pass
    | _ -> state |> pass
