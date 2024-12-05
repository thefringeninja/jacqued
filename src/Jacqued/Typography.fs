namespace Jacqued

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
    let headline5 = Typography.create
    let headline6 = Typography.create
    let subtitle1 = Typography.create
    let subtitle2 = Typography.create
    let body1 = Typography.create
    let body2 = Typography.create
    let button = Typography.create
