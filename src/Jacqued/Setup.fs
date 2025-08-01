﻿module Jacqued.Setup

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Input
open Avalonia.Input.TextInput
open Avalonia.Layout
open Avalonia.Styling
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Util
open Material.Icons

type State =
    { Bar: Bar
      Plates: PlatePair list
      PlateToAdd: Weight
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      SelectedTheme: ThemeVariant }

    static member zero =
        { Bar = Bar.zero
          Plates = []
          PlateToAdd = Weight.zero
          MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          SelectedTheme = ThemeVariant.Default }

let update handler msg (state: State) =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                Plates = e.Plates
                MeasurementSystem = e.MeasurementSystem }
            |> pass
        | _ -> state |> pass
    | SetupGym(bar, plates, units, days) ->
        state,
        handler (
            Command.SetupGym(
                { Bar = bar
                  Plates = plates
                  MeasurementSystem = units
                  ExerciseDaysPerWeek = days }
                : SetupGym
            )
        )
    | ExerciseDaysPerWeekChanged daysPerWeek ->
        { state with
            ExerciseDaysPerWeek = daysPerWeek }
        |> pass
    | MeasurementSystemChanged system ->
        { state with
            MeasurementSystem = system }
        |> pass
    | BarbellWeightChanged weight -> { state with Bar = Bar.Of(weight) } |> pass
    | PlateWeightChanged weight -> { state with PlateToAdd = weight } |> pass
    | AddPlate weight ->
        let platePairs = state.Plates |> List.append [ PlatePair(weight) ]

        { state with
            Plates = platePairs
            PlateToAdd = Weight.zero }
        |> pass
    | RemovePlate weight ->
        { state with
            Plates =
                state.Plates
                |> List.removeAt (state.Plates |> List.findIndex (fun plate -> plate.WeightOfEach = weight)) }
        |> pass
    | SelectTheme theme -> { state with SelectedTheme = theme } |> pass
    | ConfigurationSettingsLoaded { ThemeVariant = theme } -> { state with SelectedTheme = theme } |> pass
    | _ -> state |> pass

let private radioButtonGroup (format: 't -> string) items selected label groupName action =
    let label = Typography.body2 label |> generalize

    let radioButtons =
        items
        |> List.map (
            (fun item ->
                let isChecked = selected = item

                RadioButton.create [
                    RadioButton.content (format item)
                    RadioButton.groupName groupName
                    RadioButton.isChecked isChecked
                    RadioButton.onChecked (fun _ -> item |> action)
                ])
            >> generalize
        )

    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children ([ [ label ]; radioButtons ] |> List.concat)
    ]
    |> generalize

let private themeSelector state dispatch =
    let themes = [ ThemeVariant.Default; ThemeVariant.Light; ThemeVariant.Dark ]

    let format (item: ThemeVariant) = item.Key |> string

    radioButtonGroup format themes state.SelectedTheme "Theme" (nameof ThemeVariant) (Msg.SelectTheme >> dispatch)

let private gymSetup (state: State) (dispatch: Msg -> unit) =
    let onBarbellWeightChange s =
        let msg =
            match Weight.tryParse s with
            | Ok weight -> weight |> BarbellWeightChanged
            | Result.Error error -> error |> Message |> ApplicationError

        msg |> dispatch

    let onPlateWeightChange s =
        let msg =
            match Weight.tryParse s with
            | Ok weight -> weight |> PlateWeightChanged
            | Result.Error error -> error |> Message |> ApplicationError

        msg |> dispatch

    let onPlateAdd weight _ =
        let msg =
            if weight > Weight.zero then
                weight |> AddPlate
            else
                "Weight must be greater than 0" |> Message |> ApplicationError

        msg |> dispatch

    let onPlateKeyDown weight (e: KeyEventArgs) =
        if e.Key <> Key.Return then
            ()
        else
            onPlateAdd weight e
            e.Handled <- true

    let onPlateRemove = RemovePlate >> dispatch

    let setupGymEnabled = state.Bar.Weight > Weight.zero && state.Plates.Length > 0

    let onSetupGym _ =
        (state.Bar, state.Plates, state.MeasurementSystem, state.ExerciseDaysPerWeek)
        |> Msg.SetupGym
        |> dispatch

    let setupGym =
        MaterialButton.create [
            MaterialButton.dock Dock.Right
            MaterialButton.content ("Setup gym", MaterialIconKind.Check)
            MaterialButton.isEnabled setupGymEnabled
            MaterialButton.onClick (onSetupGym, SubPatchOptions.OnChangeOf state)
        ]

    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            let format item = $"{item}"

            radioButtonGroup
                format
                ExerciseDaysPerWeek.all
                state.ExerciseDaysPerWeek
                "Exercise days"
                (nameof ExerciseDaysPerWeek)
                (ExerciseDaysPerWeekChanged >> dispatch)

            radioButtonGroup
                format
                MeasurementSystem.all
                state.MeasurementSystem
                "Units"
                (nameof MeasurementSystem)
                (MeasurementSystemChanged >> dispatch)

            TextBox.create [
                TextBox.label "Barbell"
                TextBox.contentType TextInputContentType.Number
                TextBox.text $"{state.Bar.Weight}"
                TextBox.onTextChanged onBarbellWeightChange
                TextBox.margin (0, 0, 0, 16)
            ]

            WrapPanel.create [
                WrapPanel.children [
                    yield!
                        PlatePairs.control (state.MeasurementSystem, state.Plates, onPlateRemove, SubPatchOptions.OnChangeOf state.Plates)

                    yield
                        TextBox.create [
                            TextBox.minWidth 50
                            TextBox.contentType TextInputContentType.Number
                            TextBox.text $"{state.PlateToAdd}"
                            TextBox.onTextChanged onPlateWeightChange
                            TextBox.onKeyDown ((onPlateKeyDown state.PlateToAdd), SubPatchOptions.OnChangeOf state.PlateToAdd)
                            TextBox.margin (0, 0, 0, 16)
                        ]
                ]
            ]

            buttonBar [ setupGym ]
        ]
    ]

let view state dispatch =
    let content =
        StackPanel.create [
            StackPanel.children [ themeSelector state dispatch; Separator.create []; gymSetup state dispatch ]
        ]

    layout content
