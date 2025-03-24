module Jacqued.Util

open System

let pascalize (s: string) =
    s
    |> Seq.mapi (fun i c -> if i = 0 then Char.ToUpper c else c)
    |> Seq.map string
    |> String.concat ""

let toDateTime (date:DateOnly) = date.ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.Zero))

module DateOnly =
    
    let epoch = DateTime.UnixEpoch |> DateOnly.FromDateTime 
type DateOnly with 
    static member today
        with get() = DateTime.Today |> DateOnly.FromDateTime
