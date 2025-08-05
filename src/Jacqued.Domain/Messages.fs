namespace Jacqued

open System

type Command =
    | SetupGym of SetupGym
    | CalculateOneRepMax of CalculateOneRepMax
    | StartMesocycle of StartMesocycle
    | CompleteRepSet of CompleteRepSet
    | CompleteWave of CompleteWave
    | FailRepSet of FailRepSet

and SetupGym =
    { Bar: Bar
      Plates: PlatePair list
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

and CalculateOneRepMax =
    { Weight: Weight
      Reps: uint32
      Exercise: Exercise
      CalculatedOn: DateOnly }    

and StartMesocycle =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      OneRepMax: Weight
      StartedAt: DateOnly
      Bar: Bar
      Plates: PlatePair list
      MeasurementSystem: MeasurementSystem }

and CompleteRepSet =
    { MesocycleId: MesocycleId
      Reps: uint
      Weight: Weight
      CompletedAt: DateOnly }
and CompleteWave =
    { MesocycleId: MesocycleId
      CompletedAt: DateOnly }

and FailRepSet =
    { MesocycleId: MesocycleId
      Reps: uint
      Weight: Weight
      FailedAt: DateOnly }

type Event =
    | GymSetup of GymSetup
    | OneRepMaxCalculated of OneRepMaxCalculated 
    | MesocycleStarted of MesocycleStarted
    | RepSetCompleted of RepSetCompleted
    | WaveCompleted of WaveCompleted
    | MesocycleFailed of MesocycleFailed
    | MesocycleCompleted of MesocycleCompleted

and GymSetup =
    { Bar: Bar
      Plates: PlatePair list
      MeasurementSystem: MeasurementSystem
      ExercisesDaysPerWeek: ExerciseDaysPerWeek }

and OneRepMaxCalculated =
    { OneRepMax: Weight
      Exercise: Exercise
      CalculatedOn: DateOnly }

and MesocycleStarted =
    { MesocycleId: MesocycleId
      OneRepMax: Weight
      TrainingOneRepMax: Weight
      StartedAt: DateOnly
      WorkoutPlan: WorkoutPlan
      MeasurementSystem: MeasurementSystem }

and RepSetCompleted =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      Weight: Weight
      Wave: Wave
      RepSet: RepSet
      Reps: uint
      CompletedAt: DateOnly }

and WaveCompleted =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      Wave: Wave
      CompletedAt: DateOnly }

and MesocycleFailed =
    { MesocycleId: MesocycleId
      Exercise: Exercise
      Weight: Weight
      SuggestedOneRepMax: Weight
      Wave: Wave
      RepSet: RepSet
      Reps: uint
      FailedAt: DateOnly }

and MesocycleCompleted =
    { MesocycleId: MesocycleId
      SuggestedOneRepMax: Weight
      Exercise: Exercise }
