module Jacqued.AssistanceTemplate

open System

type State =
    { Name: string option
      WasRemoved: bool }

    static member zero = { Name = None; WasRemoved = false }

let private define (command: DefineAssistanceTemplate) (hasAssistanceTemplate: string -> bool) state =
    if command.Name = String.Empty then
        invalidArg (nameof command) "Name cannot be empty"

    if state.WasRemoved then invalidOp "Assistance template was deleted"

    if
        Exercise.all
        |> List.exists (fun e -> not (command.Exercises |> Map.containsKey e))
    then
        invalidArg (nameof command) "Assistance template is missing one or more exercises"

    if (command.Name |> Some) <> state.Name && hasAssistanceTemplate command.Name then
        invalidOp $"Assistance Template named {command.Name} already exists"

    [ AssistanceTemplateDefined
          { AssistanceTemplateId = command.AssistanceTemplateId
            Name = command.Name
            Exercises = command.Exercises } ]

let private remove (command: RemoveAssistanceTemplate) state =
    if state.WasRemoved then
        []
    else
        [ AssistanceTemplateRemoved { AssistanceTemplateId = command.AssistanceTemplateId } ]

let handle hasAssistanceTemplate =
    function
    | DefineAssistanceTemplate command -> define command hasAssistanceTemplate
    | RemoveAssistanceTemplate command -> remove command
    | _ -> invalidOp "invalid command"

let evolve state =
    function
    | AssistanceTemplateDefined e -> { state with Name = e.Name |> Some }: State
    | AssistanceTemplateRemoved e -> { state with WasRemoved = false }
    | _ -> state
