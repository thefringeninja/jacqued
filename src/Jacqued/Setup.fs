module Jacqued.Setup

open Avalonia
open Avalonia.Input.TextInput
open Jacqued.Controls
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Helpers
open Avalonia.Layout
open Avalonia.Media
open Material.Icons
open Material.Icons.Avalonia
open Material.Styles.Controls

type State =
    { Bar: Bar
      Plates: PlatePair list
      PlateToAdd: Weight
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

    static member zero =
        { Bar = Bar.Of(Weight.zero)
          Plates = []
          PlateToAdd = Weight.zero
          MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four }

let update msg state handler : State * Result<Event list, exn> =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Bar = e.Bar
                ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
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
        { state with
            Plates = state.Plates |> List.append [ PlatePair(state.PlateToAdd) ]
            PlateToAdd = Weight.zero },
        List.empty |> Ok
    | RemovePlate index ->
        { state with
            Plates = state.Plates |> List.removeAt index },
        List.empty |> Ok
    | _ -> state, List.empty |> Ok

let view (state: State) (dispatch: Msg -> unit) =
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

    let onPlateRemove index _ =
        let msg = RemovePlate index

        msg |> dispatch

    let setupGymEnabled = state.Bar.Weight > Weight.zero && state.Plates.Length > 0

    let onSetupGym _ =
        (state.Bar, state.Plates, state.MeasurementSystem, state.ExerciseDaysPerWeek)
        |> Msg.SetupGym
        |> dispatch

    let radioButtonGroup items selected label groupName action =
        let label =
            TextBlock.create [ TextBlock.text label; TextBlock.fontWeight FontWeight.Bold ]
            |> generalize

        let radioButtons =
            items
            |> List.map (fun item ->
                let isChecked = selected = item

                RadioButton.create
                    [ RadioButton.content $"{item}"
                      RadioButton.groupName groupName
                      RadioButton.isChecked isChecked
                      RadioButton.onChecked (fun _ -> item |> action) ])
            |> List.map generalize

        StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.children ([ [ label ]; radioButtons ] |> List.concat) ]
        |> generalize

    let content =
        StackPanel.create
            [ StackPanel.orientation Orientation.Vertical
              StackPanel.children
                  [ yield
                        radioButtonGroup
                            ExerciseDaysPerWeek.all
                            state.ExerciseDaysPerWeek
                            "Exercise days"
                            (nameof ExerciseDaysPerWeek)
                            (fun days -> days |> ExerciseDaysPerWeekChanged |> dispatch)
                    yield
                        radioButtonGroup MeasurementSystem.all state.MeasurementSystem "Units" (nameof MeasurementSystem) (fun units ->
                            units |> MeasurementSystemChanged |> dispatch)

                    yield
                        TextBox.create
                            [ TextBox.label "Barbell"
                              TextBox.contentType TextInputContentType.Number
                              TextBox.text $"{state.Bar.Weight}"
                              TextBox.onTextChanged onBarbellWeightChange ]

                    yield!
                        state.Plates
                        |> Seq.mapi (fun index plate ->
                            DockPanel.create
                                [ DockPanel.children
                                      [ Button.create
                                            [ Button.dock Dock.Right
                                              Button.onClick ((onPlateRemove index), SubPatchOptions.OnChangeOf state.Plates)
                                              Button.cornerRadius 20
                                              Button.height 40
                                              Button.width 40
                                              Button.content (MaterialIcon.create [ MaterialIcon.kind MaterialIconKind.Minus ]) ]
                                        TextBlock.create [ TextBlock.text $"{plate.WeightOfEach} {state.MeasurementSystem}" ] ] ]
                            |> generalize)

                    yield
                        DockPanel.create
                            [ DockPanel.children
                                  [ Button.create
                                        [ Button.dock Dock.Right
                                          Button.cornerRadius 20
                                          Button.height 40
                                          Button.width 40
                                          Button.content (MaterialIcon.create [ MaterialIcon.kind MaterialIconKind.Plus ])
                                          Button.onClick ((onPlateAdd state.PlateToAdd), SubPatchOptions.OnChangeOf state.PlateToAdd) ]
                                    TextBox.create
                                        [ TextBox.label "Add plate"
                                          TextBox.contentType TextInputContentType.Number 
                                          TextBox.text $"{state.PlateToAdd}"
                                          TextBox.onTextChanged onPlateWeightChange ] ] ]

                    ] ]

    let setupGym =
        FloatingButton.create
            [ FloatingButton.content (MaterialIconKind.Check, "Setup gym")
              FloatingButton.isExtended true
              FloatingButton.isEnabled setupGymEnabled
              FloatingButton.onClick (onSetupGym, SubPatchOptions.OnChangeOf state) ]

    floatingLayout [ setupGym ] content
