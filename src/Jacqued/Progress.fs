module Jacqued.Progress

open System
open Jacqued.Controls
open LiveChartsCore
open LiveChartsCore.Defaults
open LiveChartsCore.Measure
open LiveChartsCore.SkiaSharpView
open LiveChartsCore.SkiaSharpView.Avalonia

type State =
    { Current: Map<Exercise, Weight * DateTime>
      History: Map<Exercise, (Weight * bool * DateTime) array> }

    static member zero =
        { Current =
            Exercise.all
            |> List.map (fun exercise -> (exercise, (Weight.zero, DateTime.UnixEpoch)))
            |> Map.ofList
          History = Map.empty }

let exerciseDataPoint exercise progress passed =
    progress.Current[exercise] |> fst, passed, progress.Current[exercise] |> snd

let update (msg: Msg) (state: State) : State =
    match msg with
    | Event e ->
        match e with
        | MesocycleStarted e ->
            { state with
                State.Current = state.Current |> Map.add e.WorkoutPlan.Exercise (e.OneRepMax, e.StartedAt) }
        | MesocycleFailed e ->
            let dataPoint = exerciseDataPoint e.Exercise state false

            { state with
                State.History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }
        | MesocycleCompleted e ->
            let dataPoint = exerciseDataPoint e.Exercise state true

            { state with
                State.History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }

        | _ -> state
    | _ -> state

let view state _ =
    let series (exercise, history) : ISeries =
        let columnSeries = LineSeries<DateTimePoint>()
        columnSeries.Name <- exercise.ToString()
        columnSeries.Fill <- null

        columnSeries.Values <-
            history
            |> Array.map (fun (weight: Weight, passed, date) -> DateTimePoint(date, weight.Value |> float))

        columnSeries

    CartesianChart.create [
        CartesianChart.series (state.History |> Map.toArray |> Array.map series)
        CartesianChart.legendPosition LegendPosition.Top
        CartesianChart.xaxes [ DateTimeAxis(TimeSpan.FromDays(1), (fun d -> d.ToString("M"))) ]
    ]
