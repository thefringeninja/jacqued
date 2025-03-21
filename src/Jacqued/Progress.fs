﻿module Jacqued.Progress

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Jacqued.DSL
open Jacqued.Helpers
open LiveChartsCore
open LiveChartsCore.Kernel
open LiveChartsCore.Measure
open LiveChartsCore.SkiaSharpView
open LiveChartsCore.SkiaSharpView.Avalonia
open LiveChartsCore.SkiaSharpView.Painting
open LiveChartsCore.SkiaSharpView.SKCharts
open Microsoft.FSharp.Control
open Microsoft.FSharp.Core
open SkiaSharp

type WorkoutPoint =
    { Date: DateTime
      Weight: Weight
      MesocycleNumber: uint
      Failed: bool }

type State =
    { Exercise: Exercise option
      Current: Map<Exercise, (Weight * DateTime) * uint>
      History: Map<Exercise, WorkoutPoint array> }

    static member zero =
        { Exercise = None
          Current =
            Exercise.all
            |> List.map (fun exercise -> (exercise, ((Weight.zero, DateTime.UnixEpoch), 0u)))
            |> Map.ofList
          History = Map.empty }

let exerciseDataPoint exercise date progress passed =
    { Date = date
      Weight = (progress.Current[exercise] |> fst |> fst)
      MesocycleNumber = progress.Current[exercise] |> snd
      Failed = not passed }

let update (msg: Msg) (state: State) : State =
    match msg with
    | Event e ->
        match e with
        | MesocycleStarted e ->

            { state with
                State.Current =
                    state.Current
                    |> Map.change e.WorkoutPlan.Exercise (function
                        | Some(_, number) -> ((e.TrainingOneRepMax, DateTime.UnixEpoch), number + 1u) |> Some
                        | _ -> None) }
        | MesocycleFailed e ->
            let dataPoint = exerciseDataPoint e.Exercise e.FailedAt state false

            { state with
                State.History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }
        | WaveCompleted e when e.Wave = Wave.Four ->
            let dataPoint = exerciseDataPoint e.Exercise e.CompletedAt state true

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

let errorPaint = SKColor(255uy, 0uy, 0uy) |> SolidColorPaint

let series =
    (Exercise.all,
     Resources.swatches
     |> List.take 4
     |> List.map (_.ToUInt32())
     |> List.map (SKColor >> SolidColorPaint))
    ||> List.map2 (fun exercise paint ->
        let columnSeries = LineSeries<WorkoutPoint>()
        columnSeries.Name <- exercise.ToString()
        columnSeries.Fill <- null
        columnSeries.Stroke <- paint
        columnSeries.GeometryStroke <- paint
        columnSeries.YToolTipLabelFormatter <- (fun point -> $"Mesocycle {point.Model.MesocycleNumber}, Weight: {point.Model.Weight}")
        columnSeries.Mapping <- fun workout index -> Coordinate(workout.Date.Ticks |> float, workout.Weight.Value |> float)

        columnSeries.add_PointMeasured (fun point ->
            if point.Model.Failed then
                point.Visual.Stroke <- errorPaint)

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

        let history =
            state.History
            |> Map.tryFind exercise
            |> function
                | Some history -> history
                | _ -> [||]

        columnSeries.IsVisible <- state.Exercise.IsNone || state.Exercise.Value = exercise

        columnSeries.Values <- history

    series |> Array.iteri mutateSeries

    let chart =
        CartesianChart.create [
            DockPanel.dock Dock.Bottom
            CartesianChart.series series
            CartesianChart.legendPosition LegendPosition.Top
            CartesianChart.legend chartLegend
            CartesianChart.zoomMode ZoomAndPanMode.X
            CartesianChart.xaxes [ DateTimeAxis(TimeSpan.FromDays(1), _.ToString("M")) ]
        ]

    let content =
        DockPanel.create [ DockPanel.lastChildFill true; DockPanel.children [ selector; chart ] ]

    layout content
