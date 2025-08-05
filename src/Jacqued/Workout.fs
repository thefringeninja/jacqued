[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Jacqued.Workout

open System
open Jacqued
open Jacqued.Msg
open Jacqued.Msg.Workout
open Jacqued.Workout

type Screen =
    | StartMesocycle
    | Warmup
    | CalculateOneRepMax
    | WorkingOut
    | Supplements
    | Summary

type Mesocycles = Map<Exercise, MesocycleId>

type State =
    { StartMesocycle: StartMesocycle.State
      OneRepMaxLifts: OneRepMaxLifts.State
      WarmupLifts: WarmupLifts.State
      MainLifts: MainLifts.State
      SupplementaryLifts: SupplementaryLifts.State
      Summary: Summary.State
      Screen: Screen
      Mesocycles: Mesocycles }

    static member zero =
        { StartMesocycle = StartMesocycle.State.zero
          OneRepMaxLifts = OneRepMaxLifts.State.zero
          WarmupLifts = WarmupLifts.State.zero
          MainLifts = MainLifts.State.zero
          SupplementaryLifts = SupplementaryLifts.State.zero
          Summary = Summary.State.zero
          Screen = Screen.StartMesocycle
          Mesocycles = Map.empty }

let update (now: _ -> DateOnly) handler msg (state: State) =
    let startMesocycle, startMesocycleResult =
        StartMesocycle.update now handler msg state.StartMesocycle

    let warmupLifts = WarmupLifts.update msg state.WarmupLifts

    let oneRepMaxLifts, oneRepMaxLiftsResult =
        OneRepMaxLifts.update handler msg state.OneRepMaxLifts

    let mainLifts, mainLiftsResult = MainLifts.update handler msg state.MainLifts

    let supplementaryLifts, supplementaryLiftsResult =
        SupplementaryLifts.update handler msg state.SupplementaryLifts

    let summary = Summary.update msg state.Summary

    let results =
        [ startMesocycleResult
          mainLiftsResult
          supplementaryLiftsResult
          oneRepMaxLiftsResult ]
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
                | RepSet.Complete -> Supplements
                | _ -> WorkingOut
            | WaveCompleted _ -> Screen.Summary
            | MesocycleFailed e -> nextScreen e.Exercise
            | _ -> state.Screen
        | Workout e ->
            match e with
            | WarmupLifts e ->
                match e with
                | WarmupLifts.CompleteWarmup -> Screen.WorkingOut
                | _ -> state.Screen
            | ContinueExercise exercise -> nextScreen exercise
            | OneRepMaxLifts e ->
                match e with
                | BeginCalculateOneRepMaxClicked _ -> Screen.CalculateOneRepMax
                | Complete exercise -> nextScreen exercise
                | _ -> state.Screen
            | _ -> state.Screen
        | _ -> state.Screen

    { state with
        StartMesocycle = startMesocycle
        WarmupLifts = warmupLifts
        MainLifts = mainLifts
        SupplementaryLifts = supplementaryLifts
        OneRepMaxLifts = oneRepMaxLifts
        Summary = summary
        Mesocycles = mesocycles
        Screen = screen },
    results

let view (state: State) dispatch =
    (function
    | StartMesocycle -> StartMesocycle.view state.StartMesocycle
    | CalculateOneRepMax -> OneRepMaxLifts.view state.OneRepMaxLifts
    | Warmup -> WarmupLifts.view state.WarmupLifts
    | WorkingOut -> MainLifts.view state.MainLifts
    | Supplements -> SupplementaryLifts.view state.SupplementaryLifts
    | Summary -> Summary.view state.Summary)
        state.Screen
        dispatch
