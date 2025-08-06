namespace Jacqued

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

module Typography =
    open Avalonia.FuncUI.DSL
    open Avalonia.Controls

    type private Typography =
        static member create(text: string, [<CallerMemberName; Optional; DefaultParameterValue("")>] ``class``: string) =
            TextBlock.create [ TextBlock.text text; TextBlock.classes [ ``class`` |> Util.pascalize ] ]

    let headline1 = Typography.create
    let headline2 = Typography.create
    let headline3 = Typography.create
    let headline4 = Typography.create
    let headline5 = Typography.create
    let headline6 = Typography.create
    let subtitle1 = Typography.create
    let subtitle2 = Typography.create
    let body1 = Typography.create
    let body2 = Typography.create
    let button = Typography.create

type Typography() =
    static member activity = Typography.headline4

    static member mesocycleNumber(mesocycleNumber: uint) =
        Typography.headline5 $"Mesocycle {mesocycleNumber}"

    static member currentExercise(exercise: Exercise, wave: Wave) =
        Typography.headline6 $"{exercise}, Wave {wave}"

    static member currentExercise(exercise: Exercise) = Typography.headline6 $"{exercise}"

    static member repSet(repSet: RepSet) = Typography.body2 $"Set {repSet}"
    static member repSet(repSet: int) = Typography.body2 $"Set {repSet}"

    static member weight(weight: Weight, units: MeasurementSystem) =
        Typography.body2 $"Weight: {weight}{units}"

    static member reps(reps: uint, ?amrap: bool) =
        let plus =
            match amrap with
            | Some amrap when amrap = true -> "+"
            | _ -> ""

        Typography.body2 $"Reps: {reps}{plus}"

    static member completedReps(reps: uint) =
        Typography.body2 $"Completed Reps: {reps}"

    static member oneRepMax(weight: Weight, units: MeasurementSystem) = Typography.subtitle2 $"{weight}{units}"

    static member date(date: DateOnly) = Typography.subtitle1 $"{date:d}"

    static member platePairs(weight: Weight, units: MeasurementSystem, count: int) =
        Typography.body2 $"{weight} {units} (x{count})"
