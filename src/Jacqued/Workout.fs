module Workout

open System
open Jacqued

type Screen =
    | StartMesocycle
    | Warmup
    | WorkingOut
    | Assistance
    | Summary

type Mesocycles = Map<Exercise, MesocycleId>

type State =
    { StartMesocycle: StartMesocycle.State
      WarmupLifts: WarmupLifts.State
      MainLifts: MainLifts.State
      Assistance: Assistance.State
      Summary: Summary.State
      Screen: Screen
      Mesocycles: Mesocycles }
    static member zero =
        { StartMesocycle = StartMesocycle.State.zero
          WarmupLifts = WarmupLifts.State.zero
          MainLifts = MainLifts.State.zero
          Assistance = Assistance.State.zero
          Summary = Summary.State.zero
          Screen = Screen.StartMesocycle
          Mesocycles = Map.empty }

let update (now: _ -> DateTime) handler msg (state: State) =
    let startMesocycle, startMesocycleResult =
        StartMesocycle.update now handler msg state.StartMesocycle

    let warmupLifts = WarmupLifts.update msg state.WarmupLifts
    let mainLifts, mainLiftsResult = MainLifts.update now handler msg state.MainLifts
    let assistance, assistanceResult = Assistance.update handler msg state.Assistance
    let summary = Summary.update msg state.Summary

    let results =
        [ startMesocycleResult; mainLiftsResult; assistanceResult ]
        |> List.fold
            (fun acc elem ->
                match acc with
                | Error _ -> acc
                | Ok acc ->
                    match elem with
                    | Error err -> err |> Error
                    | Ok elem -> (elem @ acc) |> Ok)
            (List.empty |> Ok)

    let nextScreen exercise =
        if state.Mesocycles |> Map.containsKey (exercise |> Exercise.next) then
            Warmup
        else
            StartMesocycle

    let mesocycles =
        match msg with
        | Event e ->
            match e with
            | MesocycleStarted e -> state.Mesocycles |> Map.add e.WorkoutPlan.Exercise e.MesocycleId
            | MesocycleCompleted e -> state.Mesocycles |> Map.remove e.Exercise
            | MesocycleFailed e -> state.Mesocycles |> Map.remove e.Exercise
            | _ -> state.Mesocycles
        | _ -> state.Mesocycles

    let screen =
        match msg with
        | Event e ->
            match e with
            | MesocycleStarted _ -> Screen.Warmup
            | RepSetCompleted e ->
                match e.RepSet |> RepSet.next with
                | RepSet.Complete -> Assistance
                | _ -> state.Screen
            | WaveCompleted _ -> Screen.Summary
            | MesocycleFailed e -> nextScreen e.Exercise
            | _ -> state.Screen
        | CompleteWarmup -> Screen.WorkingOut
        | Msg.ContinueExercise exercise -> nextScreen exercise
        | _ -> state.Screen

    { state with
        StartMesocycle = startMesocycle
        WarmupLifts = warmupLifts
        MainLifts = mainLifts
        Assistance = assistance
        Summary = summary
        Mesocycles = mesocycles
        Screen = screen },
    results

let view (state: State) dispatch =
    (function
    | StartMesocycle -> StartMesocycle.view state.StartMesocycle
    | Warmup -> WarmupLifts.view state.WarmupLifts
    | WorkingOut -> MainLifts.view state.MainLifts
    | Assistance -> Assistance.view state.Assistance
    | Summary -> Summary.view state.Summary)
        state.Screen
        dispatch
