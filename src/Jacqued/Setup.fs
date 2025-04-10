module Jacqued.Setup

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
open Material.Icons

type State =
    { Bar: Bar
      Plates: PlatePair list
      PlatePairColorIndex: PlatePair list
      PlateToAdd: Weight
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      SelectedTheme: ThemeVariant
      ActualTheme: ThemeVariant }

    static member zero =
        { Bar = Bar.zero
          Plates = []
          PlatePairColorIndex = List.empty
          PlateToAdd = Weight.zero
          MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          SelectedTheme = ThemeVariant.Default
          ActualTheme = ThemeVariant.Default }

let update handler msg state =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                PlatePairColorIndex = e.Plates |> PlatePairs.index
                Plates = e.Plates
                MeasurementSystem = e.MeasurementSystem },
            List.empty |> Ok
        | _ -> state, List.empty |> Ok
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
            ExerciseDaysPerWeek = daysPerWeek },
        List.empty |> Ok
    | MeasurementSystemChanged system ->
        { state with
            MeasurementSystem = system },
        List.empty |> Ok
    | BarbellWeightChanged weight -> { state with Bar = Bar.Of(weight) }, List.empty |> Ok
    | PlateWeightChanged weight -> { state with PlateToAdd = weight }, List.empty |> Ok
    | AddPlate weight ->
        let platePairs = state.Plates |> List.append [ PlatePair(weight) ]

        { state with
            Plates = platePairs
            PlatePairColorIndex = platePairs |> PlatePairs.index
            PlateToAdd = Weight.zero },
        List.empty |> Ok
    | RemovePlate weight ->
        { state with
            Plates =
                state.Plates
                |> List.removeAt (state.Plates |> List.findIndex (fun plate -> plate.WeightOfEach = weight)) },
        List.empty |> Ok
    | SelectTheme theme -> { state with SelectedTheme = theme }, List.empty |> Ok
    | ConfigurationSettingsLoaded { ThemeVariant = theme } -> { state with SelectedTheme = theme }, List.empty |> Ok
    | ActualThemeSelected theme -> { state with ActualTheme = theme }, List.empty |> Ok
    | _ -> state, List.empty |> Ok

let private radioButtonGroup (format: 't -> string) items selected label groupName action =
    let label = Typography.body2 label |> generalize

    let radioButtons =
        items
        |> List.map (fun item ->
            let isChecked = selected = item

            RadioButton.create [
                RadioButton.content (format item)
                RadioButton.groupName groupName
                RadioButton.isChecked isChecked
                RadioButton.onChecked (fun _ -> item |> action)
            ])
        |> List.map generalize

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

    let content =
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
                            PlatePairs.control (
                                state.MeasurementSystem,
                                state.ActualTheme,
                                state.PlatePairColorIndex,
                                state.Plates,
                                onPlateRemove,
                                SubPatchOptions.OnChangeOf state.Plates
                            )

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

    content

let view state dispatch =
    let content =
        StackPanel.create [
            StackPanel.children [ themeSelector state dispatch; Separator.create []; gymSetup state dispatch ]
        ]

    layout content
