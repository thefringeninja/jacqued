module Jacqued.Exercises

let private calculate (command: CalculateOneRepMax) =
    if command.Weight < Weight.zero then
        invalidArg (nameof command) "Weight must be positive"

    [ OneRepMaxCalculated
          { Exercise = command.Exercise
            CalculatedOn = command.CalculatedOn
            OneRepMax = Calculate.oneRepMax command.Weight command.Reps } ]

let private setWeightIncreases (command: SetWeightIncreases) =
    command.Increases.allWeightsArePositive ()

    [ WeightIncreasesSet { Increases = command.Increases } ]

let handle =
    function
    | CalculateOneRepMax command -> calculate command
    | SetWeightIncreases command -> setWeightIncreases command
    | unknown -> invalidOp $"Command {unknown.GetType().Name} not recognized"
