module Jacqued.Util

open System

let pascalize (s: string) =
    s
    |> Seq.mapi (fun i c -> if i = 0 then Char.ToUpper c else c)
    |> Seq.map string
    |> String.concat ""

