namespace Jacqued

open System

type ApplicationError =
    | Exception of exn
    | Message of string

type Msg =
    | Event of Event
    | SetupGym of Bar * PlatePair list * MeasurementSystem * ExerciseDaysPerWeek
    | BarbellWeightChanged of Weight
    | PlateWeightChanged of Weight
    | MeasurementSystemChanged of MeasurementSystem
    | ExerciseDaysPerWeekChanged of ExerciseDaysPerWeek
    | AddPlate of Weight
    | RemovePlate of int
    | StartMesocycle of MesocycleId * Exercise * Weight * DateTime
    | OneRepMaxChanged of Weight
    | StartDateChanged of DateTime
    | CompleteRepSet of MesocycleId * uint
    | FailRepSet of MesocycleId * uint
    | ApplicationError of ApplicationError
