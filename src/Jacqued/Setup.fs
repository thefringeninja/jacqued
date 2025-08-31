module Jacqued.Setup

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.TextInput
open Avalonia.Layout
open Avalonia.Styling
open Jacqued.Controls
open Jacqued.DSL
open Jacqued.Design
open Jacqued.Helpers
open Jacqued.Msg
open Jacqued.Msg.Setup
open Jacqued.Util
open Material.Icons

type AssistanceTemplateDefinition =
    { AssistanceTemplateId: AssistanceTemplateId
      Name: string
      Exercises: Map<Exercise, (Guid * AssistanceExercise) list> }

    static member zero =
        { AssistanceTemplateId = AssistanceTemplateId.Empty
          Name = ""
          Exercises = Exercise.all |> List.map (fun exercise -> (exercise, [])) |> Map.ofList }

type Gym =
    { Bar: Bar
      Plates: PlatePair list
      PlateToAdd: Weight
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

    static member zero =
        { Bar = Bar.zero
          Plates = []
          PlateToAdd = Weight.zero
          MeasurementSystem = Metric
          ExerciseDaysPerWeek = ExerciseDaysPerWeek.Four }

type AssistanceTemplate =
    { NewAssistanceTemplateName: string
      AssistanceTemplates: Map<AssistanceTemplateId, string>
      SelectedAssistanceTemplate: AssistanceTemplateDefinition option
      SelectedAssistanceTemplateExercise: Exercise option
      ListOperationInProgress: bool }

    static member zero =
        { NewAssistanceTemplateName = ""
          AssistanceTemplates = Map.empty
          SelectedAssistanceTemplate = None
          SelectedAssistanceTemplateExercise = None
          ListOperationInProgress = false }

type State =
    { Gym: Gym
      SelectedTheme: ThemeVariant
      SelectedWeightIncreases: WeightIncreases
      AssistanceTemplate: AssistanceTemplate }

    static member zero =
        { Gym = Gym.zero
          SelectedTheme = ThemeVariant.Default
          SelectedWeightIncreases = WeightIncreases.standard
          AssistanceTemplate = AssistanceTemplate.zero }

let update
    handler
    (getAssistanceTemplate:
        AssistanceTemplateId
            -> {| AssistanceTemplateId: AssistanceTemplateId
                  Name: string
                  Exercises: Map<Exercise, AssistanceExercise list> |})
    msg
    (state: State)
    =
    match msg with
    | Event e ->
        match e with
        | GymSetup e ->
            { state with
                Gym =
                    { state.Gym with
                        Bar = e.Bar
                        ExerciseDaysPerWeek = e.ExercisesDaysPerWeek
                        Plates = e.Plates
                        MeasurementSystem = e.MeasurementSystem } }
            |> pass
        | AssistanceTemplateDefined e ->
            { state with
                State.AssistanceTemplate.SelectedAssistanceTemplate = None
                State.AssistanceTemplate.AssistanceTemplates =
                    state.AssistanceTemplate.AssistanceTemplates
                    |> Map.add e.AssistanceTemplateId e.Name }
            |> pass
        | AssistanceTemplateRemoved e ->
            { state with
                State.AssistanceTemplate.SelectedAssistanceTemplate = None
                State.AssistanceTemplate.AssistanceTemplates =
                    state.AssistanceTemplate.AssistanceTemplates
                    |> Map.remove e.AssistanceTemplateId }
            |> pass
        | _ -> state |> pass
    | Setup e ->
        match e with
        | Setup.Gym e ->
            match e with
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
            | Gym.ExerciseDaysPerWeekChanged daysPerWeek ->
                { state with
                    State.Gym.ExerciseDaysPerWeek = daysPerWeek }
                |> pass
            | Gym.MeasurementSystemChanged system ->
                { state with
                    State.Gym.MeasurementSystem = system }
                |> pass
            | Gym.BarbellWeightChanged weight ->
                { state with
                    State.Gym.Bar = Bar.Of(weight) }
                |> pass
            | Gym.PlateWeightChanged weight ->
                { state with
                    State.Gym.PlateToAdd = weight }
                |> pass
            | Gym.AddPlate weight ->
                let platePairs = state.Gym.Plates |> List.append [ PlatePair(weight) ]

                { state with
                    State.Gym.Plates = platePairs
                    State.Gym.PlateToAdd = Weight.zero }
                |> pass
            | Gym.RemovePlate weight ->
                { state with
                    State.Gym.Plates =
                        state.Gym.Plates
                        |> List.removeAt (state.Gym.Plates |> List.findIndex (fun plate -> plate.WeightOfEach = weight)) }
                |> pass
        | Setup.AssistanceTemplate e ->
            match e with
            | AssistanceTemplate.DefineAssistanceTemplate(assistanceTemplateId, name, exercises) ->
                state,
                handler (
                    Command.DefineAssistanceTemplate
                        { AssistanceTemplateId = assistanceTemplateId
                          Name = name
                          Exercises = exercises }
                )
            | AssistanceTemplate.RemoveAssistanceTemplate assistanceTemplateId ->
                state, handler (Command.RemoveAssistanceTemplate { AssistanceTemplateId = assistanceTemplateId })
            | AssistanceTemplate.AssistanceTemplateDefinitionNameChanged name ->
                { state with
                    State.AssistanceTemplate.NewAssistanceTemplateName = name
                    State.AssistanceTemplate.SelectedAssistanceTemplate =
                        match state.AssistanceTemplate.SelectedAssistanceTemplate with
                        | Some assistanceTemplate -> { assistanceTemplate with Name = name } |> Some
                        | None -> None }
                |> pass
            | AssistanceTemplate.AssistanceTemplateSelected assistanceTemplateId ->
                match assistanceTemplateId with
                | Some assistanceTemplateId ->
                    let assistanceTemplate = assistanceTemplateId |> getAssistanceTemplate

                    { state with
                        State.AssistanceTemplate.SelectedAssistanceTemplate =
                            ({ AssistanceTemplateId = assistanceTemplate.AssistanceTemplateId
                               Name = assistanceTemplate.Name
                               Exercises =
                                 assistanceTemplate.Exercises
                                 |> Map.map (fun _ exercises -> exercises |> List.map (fun ae -> (Guid.NewGuid(), ae))) }
                             |> Some) }
                    |> pass
                | None ->
                    { state with
                        State.AssistanceTemplate.SelectedAssistanceTemplate = None }
                    |> pass
            | _ ->
                match state.AssistanceTemplate.SelectedAssistanceTemplate with
                | Some template ->
                    let exerciseOf =
                        (function
                        | AssistanceTemplate.Add exercise -> exercise
                        | AssistanceTemplate.Remove(exercise, _) -> exercise
                        | AssistanceTemplate.Set(exercise, _, _) -> exercise
                        | AssistanceTemplate.Reorder(exercise, _, _) -> exercise
                        | _ -> Exercise.Squats)

                    { state with
                        State.AssistanceTemplate.SelectedAssistanceTemplateExercise =
                            match e with
                            | Add exercise -> exercise |> Some
                            | _ -> state.AssistanceTemplate.SelectedAssistanceTemplateExercise
                        State.AssistanceTemplate.SelectedAssistanceTemplate =
                            { template with
                                Exercises =
                                    template.Exercises
                                    |> Map.change (exerciseOf e) (fun assistanceExercises ->
                                        match assistanceExercises with
                                        | None -> None
                                        | Some assistanceExercises ->
                                            (match e with
                                             | Add _ -> [ (Guid.NewGuid(), AssistanceExercise.zero) ] |> List.append assistanceExercises
                                             | Remove(_, index) -> assistanceExercises |> List.removeAt index
                                             | Set(_, index, assistanceExercise) ->
                                                 assistanceExercises
                                                 |> List.mapi (fun i ae -> if i = index then (ae |> fst, assistanceExercise) else ae)
                                             | Reorder(_, oldIndex, newIndex) ->
                                                 let assistanceExercise = assistanceExercises[oldIndex]

                                                 assistanceExercises
                                                 |> List.removeAt oldIndex
                                                 |> List.insertAt newIndex assistanceExercise
                                             | _ -> assistanceExercises)
                                            |> Some) }
                            |> Some }
                    |> pass
                | None ->
                    match e with
                    | AssistanceTemplate.New assistanceTemplateId ->
                        let assistanceTemplateDefinition =
                            { AssistanceTemplateDefinition.zero with
                                Name = state.AssistanceTemplate.NewAssistanceTemplateName
                                AssistanceTemplateId = assistanceTemplateId }

                        { state with
                            State.AssistanceTemplate.NewAssistanceTemplateName = ""
                            State.AssistanceTemplate.SelectedAssistanceTemplate = assistanceTemplateDefinition |> Some
                            State.AssistanceTemplate.AssistanceTemplates =
                                state.AssistanceTemplate.AssistanceTemplates
                                |> Map.add assistanceTemplateId assistanceTemplateDefinition.Name },
                        handler (
                            Command.DefineAssistanceTemplate
                                { Name = assistanceTemplateDefinition.Name
                                  AssistanceTemplateId = assistanceTemplateDefinition.AssistanceTemplateId
                                  Exercises =
                                    assistanceTemplateDefinition.Exercises
                                    |> Map.map (fun _ exercises -> exercises |> List.map snd) }
                        )
                    | _ -> state |> pass
        | Setup.WeightIncrease e ->
            match e with
            | WeightIncrease.SelectedWeightIncreasesChanged weightIncreases ->
                { state with
                    SelectedWeightIncreases = weightIncreases }
                |> pass
            | WeightIncrease.SetWeightIncreasesClick weightIncreases ->
                state, handler (Command.SetWeightIncreases({ Increases = weightIncreases }: SetWeightIncreases))
    | Settings e ->
        match e with
        | SelectTheme theme -> { state with SelectedTheme = theme } |> pass
        | ConfigurationSettingsLoaded { ThemeVariant = theme } -> { state with SelectedTheme = theme } |> pass
        | _ -> state |> pass
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

    radioButtonGroup format themes state.SelectedTheme "Theme" (nameof ThemeVariant) (Settings.SelectTheme >> Msg.Settings >> dispatch)

let private gymSetup (state: Gym) (dispatch: Msg -> unit) =
    let onBarbellWeightChange s =
        let msg =
            match Weight.tryParse s with
            | Ok weight -> weight |> Setup.Gym.BarbellWeightChanged |> Setup.Gym |> Msg.Setup
            | Result.Error error -> error |> Message |> ApplicationError

        msg |> dispatch

    let onPlateWeightChange s =
        let msg =
            match Weight.tryParse s with
            | Ok weight -> weight |> Setup.Gym.PlateWeightChanged |> Setup.Gym |> Msg.Setup
            | Result.Error error -> error |> Message |> ApplicationError

        msg |> dispatch

    let onPlateAdd weight _ =
        let msg =
            if weight > Weight.zero then
                weight |> Setup.Gym.AddPlate |> Setup.Gym |> Msg.Setup
            else
                "Weight must be greater than 0" |> Message |> ApplicationError

        msg |> dispatch

    let onPlateKeyDown weight (e: KeyEventArgs) =
        if e.Key <> Key.Return then
            ()
        else
            onPlateAdd weight e
            e.Handled <- true

    let onPlateRemove = Setup.Gym.RemovePlate >> Setup.Gym >> Msg.Setup >> dispatch

    let setupGymEnabled = state.Bar.Weight > Weight.zero && state.Plates.Length > 0

    let onSetupGym _ =
        (state.Bar, state.Plates, state.MeasurementSystem, state.ExerciseDaysPerWeek)
        |> Setup.Gym.SetupGym
        |> Setup.Gym
        |> Msg.Setup
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
                (Setup.Gym.ExerciseDaysPerWeekChanged >> Setup.Gym >> Msg.Setup >> dispatch)

            radioButtonGroup
                format
                MeasurementSystem.all
                state.MeasurementSystem
                "Units"
                (nameof MeasurementSystem)
                (Setup.Gym.MeasurementSystemChanged >> Setup.Gym >> Msg.Setup >> dispatch)

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

let private weightIncreases = [ WeightIncreases.standard; WeightIncreases.light ]

let private weightIncreasesNames =
    [ nameof WeightIncreases.standard; nameof WeightIncreases.light ]

let private weightIncreasesItemTemplate (item: Jacqued.WeightIncreases) =
    weightIncreasesNames[(weightIncreases |> List.findIndex (fun wi -> wi = item))]

let private weightIncrease (state: State) dispatch =
    let onSelectedWeightIncreasesChanged (e: obj) =
        (e :?> WeightIncreases)
        |> Setup.WeightIncrease.SelectedWeightIncreasesChanged
        |> Setup.WeightIncrease
        |> Msg.Setup
        |> dispatch

    let comboBox =
        ComboBox.create [
            ComboBox.dataItems weightIncreases
            ComboBox.itemTemplate (
                DataTemplateView<WeightIncreases>.create (weightIncreasesItemTemplate >> Typography.body2 >> centerComboBoxItem)
            )
            ComboBox.onSelectedItemChanged onSelectedWeightIncreasesChanged
            ComboBox.selectedItem state.SelectedWeightIncreases
        ]

    let onSetWeightIncreases _ =
        state.SelectedWeightIncreases
        |> Setup.WeightIncrease.SetWeightIncreasesClick
        |> Setup.WeightIncrease
        |> Msg.Setup
        |> dispatch

    let setWeightIncreases =
        MaterialButton.create [
            MaterialButton.content ("Set Weight Increases", MaterialIconKind.Barbell)
            MaterialButton.onClick (onSetWeightIncreases, SubPatchOptions.OnChangeOf state.SelectedWeightIncreases)
        ]

    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [ comboBox; buttonBar [ setWeightIncreases ] ]
    ]

let private assistanceTemplateExerciseForm (state: Exercise * int * AssistanceExercise) dispatch =
    let exercise, index, assistanceExercise = state

    let set assistanceExercise =
        (exercise, index, assistanceExercise)
        |> AssistanceTemplate.Set
        |> Setup.AssistanceTemplate
        |> Msg.Setup
        |> dispatch

    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            DockPanel.create [
                DockPanel.lastChildFill true

                DockPanel.children [
                    FlatButton.create [
                        DockPanel.dock Dock.Right
                        FlatButton.content MaterialIconKind.Delete
                        FlatButton.onClick (
                            (fun _ ->
                                (exercise, index)
                                |> Setup.AssistanceTemplate.Remove
                                |> Setup.AssistanceTemplate
                                |> Msg.Setup
                                |> dispatch),
                            SubPatchOptions.OnChangeOf assistanceExercise
                        )
                    ]

                    TextBox.create [
                        DockPanel.dock Dock.Left
                        TextBox.label "Name"
                        TextBox.text assistanceExercise.Name
                        TextBox.onTextChanged (
                            (fun s -> { assistanceExercise with Name = s } |> set),
                            SubPatchOptions.OnChangeOf assistanceExercise
                        )
                    ]
                ]
            ]
            TextBox.create [
                TextBox.label "Description"
                TextBox.maxHeight 100
                TextBox.multiline true
                TextBox.text assistanceExercise.Description
                TextBox.onTextChanged (
                    (fun s ->
                        { assistanceExercise with
                            Description = s }
                        |> set),
                    SubPatchOptions.OnChangeOf assistanceExercise
                )
            ]
            ComboBox.create [
                ComboBox.label "Base Exercise"
                ComboBox.dataItems Exercise.all
                ComboBox.itemTemplate (DataTemplateView<Exercise>.create (string >> Typography.body2 >> centerComboBoxItem))
                ComboBox.selectedItem assistanceExercise.BaseExercise
                ComboBox.onSelectedItemChanged (
                    (fun o ->
                        { assistanceExercise with
                            BaseExercise = (o :?> Exercise) }
                        |> set),
                    SubPatchOptions.OnChangeOf assistanceExercise
                )

            ]
            TextBox.create [
                TextBox.label "1RM%"
                TextBox.contentType TextInputContentType.Number
                TextBox.text $"{(assistanceExercise.PercentageOfOneRepMax * 100.):F2}"
                TextBox.onTextChanged (
                    (fun s ->
                        match s |> Double.TryParse with
                        | true, percentageOfOneRepMax ->
                            { assistanceExercise with
                                PercentageOfOneRepMax = percentageOfOneRepMax / 100. }
                        | false, _ -> assistanceExercise
                        |> set),
                    SubPatchOptions.OnChangeOf assistanceExercise
                )
            ]
            TextBox.create [
                TextBox.label "Sets"
                TextBox.contentType TextInputContentType.Digits
                TextBox.text $"{assistanceExercise.Sets}"
                TextBox.onTextChanged (
                    (fun s ->
                        match s |> UInt32.TryParse with
                        | true, sets -> { assistanceExercise with Sets = sets }
                        | false, _ -> assistanceExercise
                        |> set),
                    SubPatchOptions.OnChangeOf assistanceExercise
                )
            ]
            TextBox.create [
                TextBox.label "Reps"
                TextBox.contentType TextInputContentType.Digits
                TextBox.text $"{assistanceExercise.Reps}"
                TextBox.onTextChanged (
                    (fun s ->
                        match s |> UInt32.TryParse with
                        | true, reps -> { assistanceExercise with Reps = reps }
                        | false, _ -> assistanceExercise
                        |> set),
                    SubPatchOptions.OnChangeOf assistanceExercise
                )
            ]
        ]
    ]

let private assistanceTemplateForExercise (state: Exercise * (Guid * AssistanceExercise) list) dispatch : (Exercise * IView<Expander>) =
    let exercise, assistanceExercises = state

    (exercise,
     Expander.create [
         Expander.header (exercise |> string)
         Expander.onIsExpandedChanged (fun expanded ->
             (if expanded then exercise |> Some else None)
             |> Setup.AssistanceTemplate.AssistanceTemplateExerciseSelected
             |> Setup.AssistanceTemplate
             |> Msg.Setup
             |> dispatch)
         Expander.content (
             StackPanel.create [
                 StackPanel.orientation Orientation.Vertical
                 StackPanel.children (
                     assistanceExercises
                     |> List.mapi (fun i (id, aes) ->
                         (assistanceTemplateExerciseForm (exercise, i, aes) dispatch)
                         |> (View.withKey (id |> string))
                         |> generalize)

                 )
             ]
         )
     ])

let private assistanceTemplate (state: AssistanceTemplateDefinition * Exercise option) dispatch =
    let assistanceTemplate, selectedExercise = state

    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            yield
                DockPanel.create [
                    DockPanel.lastChildFill false
                    DockPanel.children [
                        FlatButton.create [
                            DockPanel.dock Dock.Right
                            FlatButton.content MaterialIconKind.FileRemove
                            FlatButton.onClick (
                                (fun _ ->
                                    assistanceTemplate.AssistanceTemplateId
                                    |> Setup.AssistanceTemplate.RemoveAssistanceTemplate
                                    |> Setup.AssistanceTemplate
                                    |> Msg.Setup
                                    |> dispatch),
                                SubPatchOptions.OnChangeOf assistanceTemplate.AssistanceTemplateId
                            )
                        ]
                        FlatButton.create [
                            DockPanel.dock Dock.Right
                            FlatButton.content MaterialIconKind.FileRestore
                            FlatButton.onClick (
                                (fun _ ->
                                    assistanceTemplate.AssistanceTemplateId
                                    |> Some
                                    |> Setup.AssistanceTemplate.AssistanceTemplateSelected
                                    |> Setup.AssistanceTemplate
                                    |> Msg.Setup
                                    |> dispatch),
                                SubPatchOptions.OnChangeOf assistanceTemplate.AssistanceTemplateId
                            )
                        ]
                        FlatButton.create [
                            DockPanel.dock Dock.Right
                            FlatButton.content MaterialIconKind.FileCheck
                            FlatButton.onClick (
                                (fun _ ->
                                    (assistanceTemplate.AssistanceTemplateId,
                                     assistanceTemplate.Name,
                                     assistanceTemplate.Exercises
                                     |> Map.map (fun _ exercises -> exercises |> List.map snd))
                                    |> Setup.AssistanceTemplate.DefineAssistanceTemplate
                                    |> Setup.AssistanceTemplate
                                    |> Msg.Setup
                                    |> dispatch),
                                SubPatchOptions.OnChangeOf assistanceTemplate
                            )

                        ]
                    ]
                ]
            yield!
                assistanceTemplate.Exercises
                |> Map.toList
                |> List.map (fun (exercise, aes) -> assistanceTemplateForExercise (exercise, aes) dispatch)
                |> List.map (fun (exercise, view) ->
                    [ View.withAttrs [ Expander.isExpanded ((exercise |> Some) = selectedExercise) ] view
                      |> generalize
                      buttonBar [
                          MaterialButton.create [
                              MaterialButton.content ($"{exercise} assistance", MaterialIconKind.Add)
                              MaterialButton.theme Theme.Controls.outlineButton
                              MaterialButton.onClick (fun _ ->
                                  exercise
                                  |> Setup.AssistanceTemplate.Add
                                  |> Setup.AssistanceTemplate
                                  |> Msg.Setup
                                  |> dispatch)
                          ]
                      ] ])
                |> List.concat
        ]
    ]

let private newAssistanceTemplate state dispatch =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            TextBox.create [
                DockPanel.dock Dock.Left
                TextBox.label "Name"
                TextBox.text state
                TextBox.onTextChanged (
                    Setup.AssistanceTemplate.AssistanceTemplateDefinitionNameChanged
                    >> Setup.AssistanceTemplate
                    >> Msg.Setup
                    >> dispatch
                )
            ]
            buttonBar [
                MaterialButton.create [
                    MaterialButton.content ("New", MaterialIconKind.Add)
                    MaterialButton.onClick (fun _ ->
                        AssistanceTemplateId.New()
                        |> Setup.AssistanceTemplate.New
                        |> Setup.AssistanceTemplate
                        |> Msg.Setup
                        |> dispatch)
                ]
            ]
        ]
    ]

let private assistance (state: AssistanceTemplate) dispatch =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.children [
            match state.SelectedAssistanceTemplate with
            | Some _ -> ()
            | None -> yield newAssistanceTemplate state.NewAssistanceTemplateName dispatch

            yield!
                state.AssistanceTemplates
                |> Map.toSeq
                |> Seq.map (fun (assistanceTemplateId, name) ->
                    Expander.create [
                        yield
                            Expander.onIsExpandedChanged (fun expanded ->
                                (if expanded then assistanceTemplateId |> Some else None)
                                |> Setup.AssistanceTemplate.AssistanceTemplateSelected
                                |> Setup.AssistanceTemplate
                                |> Msg.Setup
                                |> dispatch)
                        match state.SelectedAssistanceTemplate with
                        | Some assistanceTemplateDefinition when assistanceTemplateDefinition.AssistanceTemplateId = assistanceTemplateId ->
                            yield
                                Expander.header (
                                    TextBox.create [
                                        TextBox.label "Name"
                                        TextBox.text name
                                        TextBox.onTextChanged (
                                            Setup.AssistanceTemplate.AssistanceTemplateDefinitionNameChanged
                                            >> Setup.AssistanceTemplate
                                            >> Msg.Setup
                                            >> dispatch
                                        )
                                    ]
                                )

                            yield Expander.isExpanded true

                            yield
                                Expander.content (
                                    assistanceTemplate (assistanceTemplateDefinition, state.SelectedAssistanceTemplateExercise) dispatch
                                )
                        | _ ->
                            yield Expander.header name
                            yield Expander.isExpanded false
                    ]
                    |> generalize)
        ]
    ]

let view state dispatch =
    let content =
        StackPanel.create [
            StackPanel.children [
                themeSelector state dispatch
                Separator.create []
                gymSetup state.Gym dispatch
                Separator.create []
                weightIncrease state dispatch
                Separator.create []
                assistance state.AssistanceTemplate dispatch
            ]
        ]

    layout content
