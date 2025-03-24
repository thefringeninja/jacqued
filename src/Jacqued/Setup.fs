module Jacqued.Setup

open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Input.TextInput
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Styling
open Jacqued.DSL
open Jacqued.Helpers
open Material.Icons
open Material.Icons.Avalonia

type State =
    { Bar: Bar
      Plates: PlatePair list
      PlatePairColors: Map<Weight, Color>
      PlateToAdd: Weight
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek
      SelectedTheme: ThemeVariant }

    static member zero =
        { Bar = Bar.zero
          Plates = []
          PlatePairColors = Map.empty
          PlateToAdd = Weight.zero
          MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four
          SelectedTheme = ThemeVariant.Default }

let update handler msg state =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                PlatePairColors = e.Plates |> PlatePairs.colorMap
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
    | AddPlate _ ->
        let platePairs = state.Plates |> List.append [ PlatePair(state.PlateToAdd) ]

        { state with
            Plates = platePairs
            PlatePairColors = platePairs |> PlatePairs.colorMap
            PlateToAdd = Weight.zero },
        List.empty |> Ok
    | RemovePlate weight ->
        { state with
            Plates =
                state.Plates
                |> List.removeAt (state.Plates |> List.findIndex (fun plate -> plate.WeightOfEach = weight)) },
        List.empty |> Ok
    | SelectedThemeChanged theme -> { state with SelectedTheme = theme }, List.empty |> Ok
    | _ -> state, List.empty |> Ok

let radioButtonGroup (format: 't -> string) items selected label groupName action =
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

    radioButtonGroup format themes state.SelectedTheme "Theme" (nameof ThemeVariant) (fun theme ->
        theme |> Msg.SelectedThemeChanged |> dispatch)

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

    let onPlateRemove weight =
        let msg = RemovePlate weight

        msg |> dispatch

    let setupGymEnabled = state.Bar.Weight > Weight.zero && state.Plates.Length > 0

    let onSetupGym _ =
        (state.Bar, state.Plates, state.MeasurementSystem, state.ExerciseDaysPerWeek)
        |> Msg.SetupGym
        |> dispatch

    let setupGym =
        MaterialButton.create [
            Button.dock Dock.Right
            Button.content ("Setup gym", MaterialIconKind.Check)
            Button.isEnabled setupGymEnabled
            Button.onClick (onSetupGym, SubPatchOptions.OnChangeOf state)
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
                    (fun days -> days |> ExerciseDaysPerWeekChanged |> dispatch)

                radioButtonGroup format MeasurementSystem.all state.MeasurementSystem "Units" (nameof MeasurementSystem) (fun units ->
                    units |> MeasurementSystemChanged |> dispatch)

                TextBox.create [
                    TextBox.label "Barbell"
                    TextBox.contentType TextInputContentType.Number
                    TextBox.text $"{state.Bar.Weight}"
                    TextBox.onTextChanged onBarbellWeightChange
                ]

                DockPanel.create [
                    DockPanel.children [
                        Button.create [
                            Button.dock Dock.Right
                            Button.cornerRadius 20
                            Button.height 40
                            Button.width 40
                            Button.content (MaterialIcon.create [ MaterialIcon.kind MaterialIconKind.Plus ])
                            Button.onClick ((onPlateAdd state.PlateToAdd), SubPatchOptions.OnChangeOf state.PlateToAdd)
                        ]
                        TextBox.create [
                            TextBox.label "Add plate"
                            TextBox.contentType TextInputContentType.Number
                            TextBox.text $"{state.PlateToAdd}"
                            TextBox.onTextChanged onPlateWeightChange
                        ]
                    ]
                ]

                PlatePairs.control (
                    state.MeasurementSystem,
                    state.PlatePairColors,
                    state.Plates,
                    onPlateRemove,
                    SubPatchOptions.OnChangeOf state.Plates
                )

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
