namespace Jacqued

open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Avalonia.Styling

type SettingsData =
    { ThemeVariant: string option }

    static member zero = { ThemeVariant = None }

type Settings =
    { ThemeVariant: ThemeVariant
      SettingsPath: string option }

    static member zero =
        { ThemeVariant = ThemeVariant.Default
          SettingsPath = None }

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

            { ThemeVariant = themeVariant
              SettingsPath = None }
        | None -> Settings.zero

    static member options =
        let options =
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

        options

    static member load(reader: StreamReader) =
        let data =
            JsonSerializer.Deserialize<SettingsData>(reader.ReadToEnd(), Settings.options)
            |> Some

        Settings.create data

    member this.save(writer: StreamWriter) =
        let data: SettingsData =
            { ThemeVariant = this.ThemeVariant.Key.ToString().ToLowerInvariant() |> Some }

        writer.Write(JsonSerializer.Serialize(data, Settings.options))
