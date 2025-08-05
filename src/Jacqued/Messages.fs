﻿namespace Jacqued

open System
open Avalonia.Styling

module Msg =
    type Setup =
        | SetupGym of Bar * PlatePair list * MeasurementSystem * ExerciseDaysPerWeek
        | BarbellWeightChanged of Weight
        | PlateWeightChanged of Weight
        | MeasurementSystemChanged of MeasurementSystem
        | ExerciseDaysPerWeekChanged of ExerciseDaysPerWeek
        | AddPlate of Weight
        | RemovePlate of Weight    

    type Data =
        | BeginBackup
        | CompleteBackup of Result<unit, string>
        | BeginRestore
        | CompleteRestore of Result<unit, string>

    module Workout =
        type Mesocycle =
            | StartMesocycle of MesocycleId * Exercise * Weight * DateOnly * Bar * PlatePair list
            | OneRepMaxChanged of Weight
            | StartDateChanged of DateOnly
            
        type MainLifts =
            | CompleteRepSet of MesocycleId * uint * Weight
            | IncreaseReps
            | DecreaseReps
            | FailRepSet of MesocycleId * uint * Weight

        type WarmupLifts =
            | ExerciseDateChanged of DateOnly
            | CompleteWarmup

        type OneRepMaxLifts =
            | BeginCalculateOneRepMaxClicked of Exercise
            | OneRepMaxChanged of Weight
            | ExerciseDateChanged of DateOnly
            | OneRepMaxEstimated
            | CompleteWarmup
            | IncreaseReps
            | DecreaseReps
            | CompleteCalculateOneRepMaxClicked of Exercise * uint * Weight * DateOnly
            | Complete of Exercise
            
        type SupplementaryLifts =
            | SelectedSupplementaryLiftsIndexChanged of int
        
    type Workout =
        | Mesocycle of Workout.Mesocycle
        | MainLifts of Workout.MainLifts
        | WarmupLifts of Workout.WarmupLifts
        | OneRepMaxLifts of Workout.OneRepMaxLifts
        | SupplementaryLifts of Workout.SupplementaryLifts
        | ContinueExercise of Exercise
        | CompleteWave of MesocycleId * DateOnly

    type Progress =
        | SelectedProgressChartExerciseChanged of Exercise option
        | ExerciseSummaryClicked of MesocycleId * Exercise * uint
        | ExerciseDetailDismissed
        

    type Settings =
        | SelectTheme of ThemeVariant
        | ActualThemeSelected of ThemeVariant
        | ConfigurationSettingsLoaded of Jacqued.Settings
        
type ApplicationError =
    | Exception of exn
    | Message of string

type Msg =
    | Event of Event
    | Setup of Msg.Setup
    | Data of Msg.Data
    | Workout of Msg.Workout
    | Progress of Msg.Progress
    | Settings of Msg.Settings
    | ApplicationError of ApplicationError
