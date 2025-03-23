module Jacqued.Configuration

open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Avalonia.Styling

let private options =
    JsonFSharpOptions()
        .WithUnionAdjacentTag()
        .WithUnionNamedFields()
        .WithUnionUnwrapRecordCases()
        .WithUnwrapOption()
        .WithUnionUnwrapSingleCaseUnions()
        .WithUnionAllowUnorderedTag()
        .WithUnionUnwrapFieldlessTags()
        .WithSkippableOptionFields()
        .ToJsonSerializerOptions()

options.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
type SettingsData =
    { ThemeVariant: string option }

    static member zero = { ThemeVariant = None }

type Settings =
    { ThemeVariant: ThemeVariant }

    static member zero = { ThemeVariant = ThemeVariant.Default }

    static member create(data: SettingsData option) =
        match data with
        | Some data ->
            let themeVariant =
                match data.ThemeVariant with
                | Some name ->
                    match name.ToLowerInvariant() with
                    | "light" -> ThemeVariant.Light
                    | "dark" -> ThemeVariant.Dark
                    | _ -> ThemeVariant.Default
                | None -> ThemeVariant.Default

            { ThemeVariant = themeVariant } 
        | None -> Settings.zero
    
    static member load (reader:StreamReader) =
        let data = JsonSerializer.Deserialize<SettingsData>(reader.ReadToEnd(), options) |> Some
        Settings.create data
        
    member this.save (writer:StreamWriter) =
        let data:SettingsData = { ThemeVariant = this.ThemeVariant.Key.ToString().ToLowerInvariant() |> Some  }
        writer.Write(JsonSerializer.Serialize(data, options))


let load path =
    let fileInfo = FileInfo(path)

    if fileInfo.Exists then
        try
            use reader = fileInfo.OpenText()
            Settings.load reader
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
