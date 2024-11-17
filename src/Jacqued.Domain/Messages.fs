namespace Jacqued

open System

type Command =
    | SetupGym of SetupGym
    | StartMesocycle of StartMesocycle
    | CompleteRepSet of CompleteRepSet
    | FailRepSet of FailRepSet

and SetupGym =
    { Bar: Bar
      Plates: PlatePair list
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

and StartMesocycle =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      OneRepMax: Weight
      StartedAt: DateTime
      MeasurementSystem: MeasurementSystem }

and CompleteRepSet =
    { MesocycleId: MesocycleId; Reps: uint }

and FailRepSet =
    { MesocycleId: MesocycleId
      Reps: uint }

type Event =
    | GymSetup of GymSetup
    | MesocycleStarted of MesocycleStarted
    | RepSetCompleted of RepSetCompleted
    | MesocycleFailed of MesocycleFailed
    | MesocycleCompleted of MesocycleCompleted

and GymSetup =
    { Bar: Bar
      Plates: PlatePair list
      MeasurementSystem: MeasurementSystem
      ExercisesDaysPerWeek: ExerciseDaysPerWeek }

and OneRepMaxCalculated =
    { OneRepMax: Weight
      TrainingOneRepMax: Weight
      Exercise: Exercise }

and MesocycleStarted =
    { MesocycleId: MesocycleId
      OneRepMax: Weight
      StartedAt: DateTime
      WorkoutPlan: WorkoutPlan
      MeasurementSystem: MeasurementSystem }

and RepSetCompleted =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      Weight: Weight
      Wave: Wave
      RepSet: RepSet
      Reps: uint }

and MesocycleFailed =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      Weight: Weight
      SuggestedOneRepMax: Weight
      Wave: Wave
      RepSet: RepSet
      Reps: uint }

and MesocycleCompleted =
    { MesocycleId: MesocycleId
      SuggestedOneRepMax: Weight
      Exercise: Exercise }
