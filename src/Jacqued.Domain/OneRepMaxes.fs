module Jacqued.OneRepMaxes

let private calculate (command:CalculateOneRepMax) =
    if command.Weight < Weight.zero then
        invalidArg (nameof command) "Weight must be positive"

    [ OneRepMaxCalculated
          { Exercise = command.Exercise
            CalculatedOn = command.CalculatedOn
            OneRepMax = Calculate.oneRepMax command.Weight command.Reps } ]

let handle =
    function
    | CalculateOneRepMax command -> calculate command
    | unknown -> invalidOp $"Command {unknown.GetType().Name} not recognized"
