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
    | Backup
    | BeginRestore
    | CompleteRestore
    | CompleteRestoreFailed of string
    | StartMesocycle of MesocycleId * Exercise * Weight * DateOnly * Bar * PlatePair list
    | OneRepMaxChanged of Weight
    | StartDateChanged of DateOnly
    | CompleteWarmup
    | CompleteRepSet of MesocycleId * uint * Weight
    | ExerciseDateChanged of DateOnly
    | IncreaseReps
    | DecreaseReps
    | CompleteWave of MesocycleId * DateOnly
    | FailRepSet of MesocycleId * uint * Weight
    | ContinueExercise of Exercise
    | SelectedAssistanceWorkIndexChanged of int
    | SelectedProgressChartExerciseChanged of Exercise option
    | ExerciseSummaryClicked of MesocycleId * Exercise * uint
    | ExerciseDetailDismissed
    | SelectTheme of ThemeVariant
    | ActualThemeSelected of ThemeVariant
    | ConfigurationSettingsLoaded of Settings
    | ApplicationError of ApplicationError
