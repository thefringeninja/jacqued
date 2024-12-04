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
    | RemovePlate of Weight
    | StartMesocycle of MesocycleId * Exercise * Weight * DateTime
    | OneRepMaxChanged of Weight
    | StartDateChanged of DateTime
    | CompleteRepSet of MesocycleId * uint
    | ExerciseDateChanged of DateTime
    | IncreaseReps
    | DecreaseReps
    | CompleteWave of MesocycleId
    | FailRepSet of MesocycleId * uint
    | ContinueExercise
    | SelectedAssistanceWorkIndexChanged of int
    | SelectedProgressChartExerciseChanged of Exercise option
    | ApplicationError of ApplicationError
