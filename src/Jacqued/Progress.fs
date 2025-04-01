module Jacqued.Progress

open System
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Styling
open Jacqued.DSL
open Jacqued.Helpers
open Jacqued.Util
open LiveChartsCore
open LiveChartsCore.Kernel
open LiveChartsCore.Measure
open LiveChartsCore.Painting
open LiveChartsCore.SkiaSharpView
open LiveChartsCore.SkiaSharpView.Avalonia
open LiveChartsCore.SkiaSharpView.Drawing.Geometries
open LiveChartsCore.SkiaSharpView.Painting
open LiveChartsCore.SkiaSharpView.SKCharts
open Material.Colors.Recommended
open Material.Icons
open Material.Styles.Themes
open Material.Styles.Themes.Base
open Microsoft.FSharp.Core
open SkiaSharp

let colors =
    Resources.swatches |> List.take 4 |> List.map _.ToUInt32() |> List.map SKColor

type private ChartTheme =
    { LegendTextPaint: Paint
      LegendBackgroundPaint: Paint
      TooltipTextPaint: Paint
      TooltipBackgroundPaint: Paint
      GeometryFillPaint: Paint }

let private thematize (settings: LiveChartsSettings, avaloniaTheme: IBaseTheme) =
    let chartTheme = settings.GetTheme()

    let bodyPaint = avaloniaTheme.MaterialBodyColor.ToPaint()
    let backgroundPaint = avaloniaTheme.MaterialPaperColor.ToPaint()

    let s = LineSeries<int>()
    (s :> ISeries).SeriesId <- 0

    chartTheme.ApplyStyleToSeries(s)

    { GeometryFillPaint = s.GeometryFill
      LegendTextPaint = bodyPaint
      LegendBackgroundPaint = backgroundPaint
      TooltipTextPaint = bodyPaint
      TooltipBackgroundPaint = backgroundPaint }

let private darkTheme =
    (LiveCharts.DefaultSettings.AddDarkTheme(), Theme.Dark) |> thematize

let private lightTheme =
    (LiveCharts.DefaultSettings.AddLightTheme(), Theme.Light) |> thematize

let private themes =
    [ (ThemeVariant.Dark.Key |> string, darkTheme)
      (ThemeVariant.Light.Key |> string, lightTheme) ]
    |> Map.ofList

type SummaryPoint =
    { MesocycleId: MesocycleId
      Date: DateOnly
      Weight: Weight
      MesocycleNumber: uint
      Failed: bool }

type Summary =
    { Current: Map<Exercise, (Weight * DateOnly) * uint>
      History: Map<Exercise, SummaryPoint array> }

    static member zero =
        { Current =
            Exercise.all
            |> List.map (fun exercise -> (exercise, ((Weight.zero, DateOnly.epoch), 0u)))
            |> Map.ofList
          History = Exercise.all |> List.map (fun exercise -> (exercise, Array.empty)) |> Map.ofList }

type DetailPoint =
    { RequiredReps: uint
      ActualReps: uint
      Weight: Weight
      Wave: Wave }

type Detail =
    { Details: Map<MesocycleId, Map<Wave, DetailPoint>>
      MesocycleId: MesocycleId option
      MesocycleNumber: uint
      Exercise: Exercise
      Bar: Bar }

    static member zero =
        { MesocycleId = None
          MesocycleNumber = 0u
          Exercise = Squats
          Details = Map.empty
          Bar = Bar.zero }

type State =
    { Exercise: Exercise option
      Summary: Summary
      Detail: Detail
      ThemeKey: string }

    static member zero =
        { Exercise = None
          Summary = Summary.zero
          Detail = Detail.zero
          ThemeKey = ThemeVariant.Default.Key |> string }

let private exerciseDataPoint mesocycleId exercise date progress passed =
    { MesocycleId = mesocycleId
      Date = date
      Weight = (progress.Current[exercise] |> fst |> fst)
      MesocycleNumber = progress.Current[exercise] |> snd
      Failed = not passed }

let private updateSummary msg (state: Summary) =
    match msg with
    | Event e ->
        match e with
        | MesocycleStarted e ->
            { state with
                Current =
                    state.Current
                    |> Map.change e.WorkoutPlan.Exercise (function
                        | Some(_, number) -> ((e.TrainingOneRepMax, DateOnly.epoch), number + 1u) |> Some
                        | _ -> None) }
        | MesocycleFailed e ->
            let dataPoint = exerciseDataPoint e.MesocycleId e.Exercise e.FailedAt state false

            { state with
                History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }
        | WaveCompleted e when e.Wave = Wave.Four ->
            let dataPoint = exerciseDataPoint e.MesocycleId e.Exercise e.CompletedAt state true

            { state with
                History =
                    state.History
                    |> Map.change e.Exercise (function
                        | Some history -> history |> Array.append [| dataPoint |] |> Some
                        | None -> [| dataPoint |] |> Some) }

        | _ -> state
    | _ -> state

let private recordReps state mesocycleId wave reps weight =
    match wave with
    | Wave.Four -> state
    | _ ->
        state
        |> Map.change mesocycleId (function
            | None -> None
            | Some detail ->
                detail
                |> Map.change wave (function
                    | None -> None
                    | Some point ->
                        { point with
                            Weight = weight
                            ActualReps = point.ActualReps + reps }
                        |> Some)
                |> Some)

let private updateDetails msg (state: Detail) =
    let recordReps = recordReps state.Details

    match msg with
    | Event e ->
        match e with
        | GymSetup e -> { state with Bar = e.Bar }
        | MesocycleStarted e ->
            { state with
                Details =
                    state.Details
                    |> Map.add
                        e.MesocycleId
                        (e.WorkoutPlan.Sets
                         |> Map.filter (fun (wave, _) _ -> wave <> Wave.Four)
                         |> Map.fold
                             (fun acc (wave, _) (_, reps) ->
                                 acc
                                 |> Map.change wave (function
                                     | None -> reps |> Some
                                     | Some r -> (r + reps) |> Some))
                             Map.empty
                         |> Map.map (fun wave reps ->
                             { Wave = wave
                               ActualReps = 0u
                               Weight = Weight.zero
                               RequiredReps = reps })) }

        | RepSetCompleted e ->
            { state with
                Details = recordReps e.MesocycleId e.Wave e.Reps e.Weight }
        | MesocycleFailed e ->
            { state with
                Details = recordReps e.MesocycleId e.Wave e.Reps e.Weight }
        | _ -> state
    | Msg.ExerciseSummaryClicked(mesocycleId, exercise, mesocycleNumber) ->
        { state with
            MesocycleId = mesocycleId |> Some
            Exercise = exercise
            MesocycleNumber = mesocycleNumber }
    | Msg.ExerciseDetailDismissed -> { state with MesocycleId = None }
    | _ -> state

let update msg state =
    let state =
        match msg with
        | Msg.SelectedProgressChartExerciseChanged exercise -> { state with Exercise = exercise }: State
        | Msg.ActualThemeSelected theme ->
            { state with
                ThemeKey = theme.Key |> string }
        | _ -> state

    { state with
        Summary = updateSummary msg state.Summary
        Detail = updateDetails msg state.Detail }

let private dataItems = [ [ None ]; Exercise.all |> List.map Some ] |> List.concat

#nowarn "FS0760"

let private errorPaint = SKColor(255uy, 0uy, 0uy) |> SolidColorPaint

type SummaryChartPoint = ChartPoint<SummaryPoint, CircleGeometry, LabelGeometry>

type private MesocycleAxis() =
    inherit
        Axis(
            UnitWidth = 1,
            Labeler =
                (fun value ->
                    let value = value |> int32

                    if value < 0 || value >= (Wave.all |> List.length) then
                        ""
                    else
                        Wave.all |> List.item value |> string),
            MinStep = 1,
            MinLimit = -1,
            MaxLimit = 2
        )

let private exercisePaints =
    (Exercise.all, colors |> List.map SolidColorPaint)
    ||> List.map2 (fun e p -> (e, p))

let private exercisePaintsMap = exercisePaints |> Map.ofList

let view (state: State) dispatch =
    let theme = themes |> Map.tryFind state.ThemeKey

    let legendText, legendBackground, tooltipText, tooltipBackground, geometryFill =
        match theme with
        | Some theme ->
            (theme.LegendTextPaint,
             theme.LegendBackgroundPaint,
             theme.TooltipTextPaint,
             theme.TooltipBackgroundPaint,
             theme.GeometryFillPaint)
        | None -> (null, null, null, null, null)

    let summaryView () =
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

        let legend = SKDefaultLegend()
        let tooltip = SKDefaultTooltip()

        let series =
            exercisePaints
            |> List.mapi (fun i (exercise, paint) ->
                let history = state.Summary.History[exercise]

                let series =
                    LineSeries<SummaryPoint>(
                        Name = (exercise |> string),
                        Fill = null,
                        Stroke = paint,
                        GeometryFill = geometryFill,
                        GeometryStroke = paint,
                        Values = history,
                        YToolTipLabelFormatter =
                            (fun (point: SummaryChartPoint) -> $"Mesocycle {point.Model.MesocycleNumber}, Weight: {point.Model.Weight}"),
                        Mapping = (fun workout index -> Coordinate(workout.Date.DateTime.Ticks |> float, workout.Weight.Value |> float)),
                        IsVisible = (state.Exercise.IsNone || state.Exercise.Value = exercise)
                    )

                (series :> ISeries).SeriesId <- i

                series.add_PointMeasured (fun point ->
                    if point.Model.Failed then
                        point.Visual.Stroke <- errorPaint)

                series.add_ChartPointPointerDown (fun chart point ->
                    (point.Model.MesocycleId, exercise, point.Model.MesocycleNumber)
                    |> Msg.ExerciseSummaryClicked
                    |> dispatch

                    tooltip.Hide(chart.CoreChart)
                    legend.Hide(chart.CoreChart))

                series)

        let minLimit =
            series
            |> List.filter _.IsVisible
            |> List.map (fun s ->
                let date =
                    match s.Values.Count with
                    | l when l >= 4 -> (s.Values |> Seq.skip 3 |> Seq.head).Date
                    | 0 -> DateOnly.today
                    | _ -> (s.Values |> Seq.last).Date

                date.AddDays(-7).DateTime.Ticks |> float)
            |> List.min

        let chart =
            CartesianChart.create [
                DockPanel.dock Dock.Bottom
                CartesianChart.series (series |> List.map (fun x -> x :> ISeries))
                CartesianChart.legendPosition LegendPosition.Top
                CartesianChart.legend legend
                CartesianChart.legendTextPaint legendText
                CartesianChart.legendBackgroundPaint legendBackground
                CartesianChart.tooltip tooltip
                CartesianChart.tooltipTextPaint tooltipText
                CartesianChart.tooltipBackgroundPaint tooltipBackground
                CartesianChart.zoomMode ZoomAndPanMode.X

                CartesianChart.xaxes [
                    DateTimeAxis(TimeSpan.FromDays(1), _.ToString("M"), LabelsPaint = legendText, MinLimit = minLimit)
                ]
                CartesianChart.yaxes [ (Axis(LabelsPaint = legendText)) ]
            ]

        [ selector |> generalize; chart ]

    let detailView () =
        let values =
            match state.Detail.MesocycleId with
            | Some mesocycleId ->
                match state.Detail.Details |> Map.tryFind mesocycleId with
                | Some detail -> detail.Values |> Array.ofSeq
                | None -> Array.empty
            | None -> Array.empty

        let actualRepsSeries =
            ColumnSeries<DetailPoint>(
                Mapping = (fun detail index -> Coordinate(index |> float, detail.ActualReps |> float)),
                Values = values,
                IgnoresBarPosition = true,
                Fill = SolidColorPaint(GreySwatch.Grey600.ToUInt32() |> SKColor)
            )

        let requiredRepsSeries =
            ColumnSeries<DetailPoint>(
                Mapping = (fun detail index -> Coordinate(index |> float, detail.RequiredReps |> float)),
                Values = values,
                IgnoresBarPosition = true,
                Fill = exercisePaintsMap[state.Detail.Exercise]
            )

        let weightSeries =
            LineSeries<DetailPoint>(
                Mapping = (fun detail index -> Coordinate(index |> float, detail.Weight.Value |> float)),
                Values = values,
                ScalesYAt = 1,
                Fill = null
            )

        let onDismissExerciseDetailClick _ = Msg.ExerciseDetailDismissed |> dispatch

        let header =
            StackPanel.create [
                DockPanel.dock Dock.Top
                StackPanel.orientation Orientation.Vertical
                StackPanel.children [
                    DockPanel.create [
                        DockPanel.lastChildFill false
                        DockPanel.children [
                            MaterialButton.create [
                                Button.dock Dock.Left
                                Button.content MaterialIconKind.ArrowBack
                                Button.theme Resources.Theme.materialFlatButton
                                Button.onClick onDismissExerciseDetailClick
                            ]
                        ]
                    ]
                    StackPanel.create [
                        StackPanel.children [
                            Typography.headline5 $"Mesocycle {state.Detail.MesocycleNumber}"
                            Typography.headline6 $"{state.Detail.Exercise}"
                        ]
                    ]
                ]
            ]

        let chart =
            CartesianChart.create [
                DockPanel.dock Dock.Bottom
                CartesianChart.series [ actualRepsSeries; requiredRepsSeries; weightSeries ]
                CartesianChart.zoomMode ZoomAndPanMode.X
                CartesianChart.xaxes [ MesocycleAxis(LabelsPaint = legendText) ]
                CartesianChart.yaxes [
                    Axis(LabelsPaint = legendText, MinLimit = 0, MaxLimit = 40)
                    Axis(LabelsPaint = legendText, Position = AxisPosition.End, ShowSeparatorLines = false)
                ]
            ]

        [ header |> generalize; chart ]

    let children =
        match state.Detail.MesocycleId with
        | Some _ -> detailView ()
        | None -> summaryView ()

    let content =
        DockPanel.create [ DockPanel.lastChildFill true; DockPanel.children children ]

    layout content
