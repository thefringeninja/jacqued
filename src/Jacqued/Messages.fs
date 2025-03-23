namespace Jacqued

open System
open Avalonia.Styling

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
    | StartMesocycle of MesocycleId * Exercise * Weight * DateTime * Bar * PlatePair list
    | OneRepMaxChanged of Weight
    | StartDateChanged of DateTime
    | CompleteWarmup
    | CompleteRepSet of MesocycleId * uint * Weight
    | ExerciseDateChanged of DateTime
    | IncreaseReps
    | DecreaseReps
    | CompleteWave of MesocycleId * DateTime
    | FailRepSet of MesocycleId * uint * Weight
    | ContinueExercise of Exercise
    | SelectedAssistanceWorkIndexChanged of int
    | SelectedProgressChartExerciseChanged of Exercise option
    | SelectedThemeChanged of ThemeVariant
    | ConfigurationSettingsLoaded of Configuration.Settings
    | ApplicationError of ApplicationError
