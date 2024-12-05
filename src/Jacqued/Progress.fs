module Jacqued.Progress

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Jacqued.DSL
open Jacqued.Helpers
open LiveChartsCore
open LiveChartsCore.Defaults
open LiveChartsCore.Measure
open LiveChartsCore.SkiaSharpView
open LiveChartsCore.SkiaSharpView.Avalonia
open LiveChartsCore.SkiaSharpView.Painting
open LiveChartsCore.SkiaSharpView.SKCharts
open SkiaSharp

type State =
    { Exercise: Exercise option
      Current: Map<Exercise, Weight * DateTime>
      History: Map<Exercise, (Weight * bool * DateTime) array> }

    static member zero =
        { Exercise = None
          Current =
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
        | RepSetCompleted e ->
            { state with
                State.Current = state.Current |> Map.add e.Exercise (e.Weight, e.CompletedAt) }
        | MesocycleFailed e ->
            let dataPoint = exerciseDataPoint e.Exercise state false

            { state with
                State.History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }
        | WaveCompleted e ->
            let dataPoint = exerciseDataPoint e.Exercise state true

            { state with
                State.History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }

        | _ -> state
    | Msg.SelectedProgressChartExerciseChanged exercise -> { state with Exercise = exercise }
    | _ -> state

let chartLegend = SKDefaultLegend()

let dataItems = [ [ None ]; Exercise.all |> List.map Some ] |> List.concat

#nowarn "FS0760"

let series =
    (Exercise.all,
     Resources.swatches
     |> List.take 4
     |> List.map (_.ToUInt32())
     |> List.map (SKColor >> SolidColorPaint))
    ||> List.map2 (fun exercise paint ->
        let columnSeries = LineSeries<DateTimePoint>()
        columnSeries.Name <- exercise.ToString()
        columnSeries.Fill <- null
        columnSeries.Stroke <- paint
        columnSeries.GeometryStroke <- paint

        columnSeries :> ISeries)
    |> List.toArray

let view state dispatch =
    let onSelectedExerciseChange (e: obj) =
        (e :?> Exercise option) |> Msg.SelectedProgressChartExerciseChanged |> dispatch

    let itemTemplate item =
        match item with
        | Some item -> item.ToString()
        | _ -> "All"
        |> Typography.body2

    let dataTemplateView = DataTemplateView<Exercise option>.create itemTemplate

    let selector =
        ComboBox.create [
            DockPanel.dock Dock.Top
            ComboBox.onSelectedItemChanged onSelectedExerciseChange
            ComboBox.dataItems dataItems
            ComboBox.itemTemplate dataTemplateView
        ]

    let mutateSeries i (columnSeries: ISeries) =
        let exercise = Exercise.all[i]
        let history = state.History[exercise]

        columnSeries.IsVisible <- state.Exercise.IsNone || state.Exercise.Value = exercise

        columnSeries.Values <-
            history
            |> Array.map (fun (weight: Weight, passed, date: DateTime) -> DateTimePoint(date.Date, weight.Value |> float))

    series |> Array.iteri mutateSeries

    let chart =
        CartesianChart.create [
            DockPanel.dock Dock.Bottom
            CartesianChart.series series
            CartesianChart.legendPosition LegendPosition.Top
            CartesianChart.legend chartLegend
            CartesianChart.xaxes [ DateTimeAxis(TimeSpan.FromDays(1), (fun d -> d.ToString("M"))) ]
        ]

    let content =
        DockPanel.create [ DockPanel.lastChildFill true; DockPanel.children [ selector; chart ] ]

    floatingLayout [] [] content
