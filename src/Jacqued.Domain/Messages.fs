namespace Jacqued

open System

type Command =
    | SetupGym of SetupGym
    | SetWeightIncreases of SetWeightIncreases
    | CalculateOneRepMax of CalculateOneRepMax
    | StartMesocycle of StartMesocycle
    | CompleteRepSet of CompleteRepSet
    | CompleteWave of CompleteWave
    | FailRepSet of FailRepSet
    | DefineAssistanceTemplate of DefineAssistanceTemplate
    | RemoveAssistanceTemplate of RemoveAssistanceTemplate

and SetupGym =
    { Bar: Bar
      Plates: PlatePair list
      MeasurementSystem: MeasurementSystem
      ExerciseDaysPerWeek: ExerciseDaysPerWeek }

and SetWeightIncreases = { Increases: WeightIncreases }

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
      CompletedAt: DateOnly
      WeightIncrease: WeightIncreases }

and FailRepSet =
    { MesocycleId: MesocycleId
      Reps: uint
      Weight: Weight
      FailedAt: DateOnly }

and DefineAssistanceTemplate =
    { AssistanceTemplateId: AssistanceTemplateId
      Name: string
      Exercises: Map<Exercise, AssistanceExercise list> }

and RemoveAssistanceTemplate =
    { AssistanceTemplateId: AssistanceTemplateId }

type Event =
    | GymSetup of GymSetup
    | WeightIncreasesSet of WeightIncreasesSet
    | OneRepMaxCalculated of OneRepMaxCalculated
    | MesocycleStarted of MesocycleStarted
    | RepSetCompleted of RepSetCompleted
    | WaveCompleted of WaveCompleted
    | MesocycleFailed of MesocycleFailed
    | MesocycleCompleted of MesocycleCompleted
    | AssistanceTemplateDefined of AssistanceTemplateDefined
    | AssistanceTemplateRemoved of AssistanceTemplateRemoved

and WeightIncreasesSet = { Increases: WeightIncreases }

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

and AssistanceTemplateDefined =
    { AssistanceTemplateId: AssistanceTemplateId
      Name: string
      Exercises: Map<Exercise, AssistanceExercise list> }

and AssistanceTemplateRemoved =
    { AssistanceTemplateId: AssistanceTemplateId }
