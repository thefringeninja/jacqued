module Jacqued.Extensions

open System

type String with
    member this.pascalize() =
        this
        |> Seq.mapi (fun i c -> if i = 0 then Char.ToUpper c else c)
        |> Seq.map string
        |> String.concat ""
