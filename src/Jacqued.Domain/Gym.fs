module Jacqued.Gym

open Microsoft.FSharp.Core

let private setup (command: SetupGym) _ =
    if command.Bar <= Bar(Weight(0m)) then
        invalidArg (nameof command) "bar must have positive weight"

    if command.Plates.Length = 0 then
        invalidArg (nameof command) "must have more than one pair of plates"

    [ GymSetup
          { Bar = command.Bar
            Plates = command.Plates
            MeasurementSystem = command.MeasurementSystem
            ExercisesDaysPerWeek = command.ExerciseDaysPerWeek } ]

let handle =
    function
    | SetupGym command -> setup command
    | unknown -> invalidOp $"Command {unknown.GetType().Name} not recognized"

let evolve _ =
    function
    | GymSetup _ -> ()
    | unknown -> invalidOp $"Event {unknown.GetType().Name} not recognized"
