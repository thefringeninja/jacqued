module Jacqued.Util

open System
open Avalonia.Media
open Avalonia.Skia
open LiveChartsCore.SkiaSharpView.Painting

let pascalize (s: string) =
    s
    |> Seq.mapi (fun i c -> if i = 0 then Char.ToUpper c else c)
    |> Seq.map string
    |> String.concat ""

module TimeOnly =
    let zero = TimeOnly.FromTimeSpan(TimeSpan.Zero)

module DateOnly =
    
    let epoch = DateTime.UnixEpoch |> DateOnly.FromDateTime 
type DateOnly with 
    static member today
        with get() = DateTime.Today |> DateOnly.FromDateTime

    member this.DateTime = this.ToDateTime(TimeOnly.zero)

type Color with
    member this.ToPaint() =
        new SolidColorPaint(this.ToSKColor())
