module Jacqued.Workout.Types

open Jacqued

type Lift =
    { RepSet: RepSet
      Reps: uint
      Weight: Weight
      Plates: PlatePair list }

    static member zero =
        { RepSet = RepSet.One
          Reps = 0u
          Weight = Weight.Zero
          Plates = [] }
