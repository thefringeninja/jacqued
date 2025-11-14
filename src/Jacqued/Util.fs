module Jacqued.Util

open System
open Avalonia.Media
open Avalonia.Skia
open Elmish
open LiveChartsCore.SkiaSharpView.Painting

let pass state = state, Cmd.none

let cmd (state, result: Result<Event list, exn>) =
    (state,
     match result with
     | Ok events -> events |> List.map (Msg.Event >> Cmd.ofMsg) |> Cmd.batch
     | Error exn -> exn |> ApplicationError.Exception |> Msg.ApplicationError |> Cmd.ofMsg)

module TimeOnly =
    let zero = TimeOnly.FromTimeSpan(TimeSpan.Zero)

module DateOnly =

    let epoch = DateTime.UnixEpoch |> DateOnly.FromDateTime

type DateOnly with
    static member today = DateTime.Today |> DateOnly.FromDateTime

    member this.DateTime = this.ToDateTime(TimeOnly.zero)

type Color with
    member this.ToPaint() = new SolidColorPaint(this.ToSKColor())
