namespace Jacqued

open System
open Microsoft.FSharp.Collections

[<Struct; CustomComparison; CustomEquality>]
type Weight(value: decimal) =
    override this.Equals(other) = value = (other :?> Weight).Value
    override this.GetHashCode() = value.GetHashCode()
    override this.ToString() = value.ToString("F2")

    interface IComparable<Weight> with
        member this.CompareTo other = compare value other.Value

    interface IComparable with
        member this.CompareTo other = compare value (other :?> Weight).Value

    member this.Value = value
    static member zero = Weight(0m)

    static member Zero = Weight(0m)

    static member tryParse(s: string) =
        match Decimal.TryParse(s) with
        | true, value when value >= 0m -> Ok(value |> Weight)
        | true, value when value < 0m -> Ok(value |> Weight)
        //| true, value when value < 0m -> Error $"%2f{value} must be greater than 0"
        | _ -> Error $"{s} is not a valid number"

    static member (*)(a: Weight, b: float) = Weight(a.Value * Convert.ToDecimal(b))
    static member (*)(a: Weight, b: int) = Weight(a.Value * Convert.ToDecimal(b))
    static member (+)(a: Weight, b: Weight) = Weight(a.Value + b.Value)
    static member (-)(a: Weight, b: Weight) = Weight(a.Value - b.Value)
    static member (%)(a: Weight, b: Weight) = Weight(a.Value % b.Value)

    static member op_Explicit(value: Weight) : float = value.Value |> float

[<Struct>]
type Bar private (value: Weight) =
    member _.Weight = value

    static member Of(value: Weight) =
        if value <= Weight.zero then
            invalidArg (nameof value) "bar must have positive weight"

        Bar(value)

    static member zero = Bar(Weight.zero)
    override this.ToString() = value.Value.ToString("F2")

[<Struct; CustomComparison; CustomEquality>]
type PlatePair(weight: Weight) =

    member this.Weight = weight * 2
    member this.WeightOfEach = weight

    override this.Equals(other) =
        weight = (other :?> PlatePair).WeightOfEach

    override this.GetHashCode() = weight.GetHashCode()
    override this.ToString() = weight.ToString()

    interface IComparable<PlatePair> with
        member this.CompareTo other = compare weight other.WeightOfEach

    interface IComparable with
        member this.CompareTo other =
            compare weight (other :?> PlatePair).WeightOfEach

type MesocycleId =
    | MesocycleId of Guid

    member this.Value = let (MesocycleId value) = this in value
    static member New() = MesocycleId(Guid.NewGuid())
    static member From value = MesocycleId(value)
    static member Empty = MesocycleId(Guid.Empty)

type ExerciseDaysPerWeek =
    | Three
    | Four

    static member all = [ Three; Four ]

type Exercise =
    | Squats
    | BenchPress
    | Deadlifts
    | OverheadPress

    static member all = [ Squats; BenchPress; Deadlifts; OverheadPress ]
    static member upper = [ BenchPress; OverheadPress ]
    static member lower = [ Squats; Deadlifts ]

    static member next exercise =
        match exercise with
        | Squats -> BenchPress
        | BenchPress -> Deadlifts
        | Deadlifts -> OverheadPress
        | OverheadPress -> Squats

    static member previous exercise =
        match exercise with
        | Squats -> OverheadPress
        | BenchPress -> Squats
        | Deadlifts -> BenchPress
        | OverheadPress -> Deadlifts

    member x.isUpper = Exercise.upper |> List.contains x
    member x.isLower = Exercise.lower |> List.contains x

type MeasurementSystem =
    | Metric
    | Imperial

    static member all = [ Metric; Imperial ]

    override this.ToString() =
        match this with
        | Metric -> "kg"
        | Imperial -> "lbs"

type RepSet =
    | One
    | Two
    | Three
    | Complete

    static member all = [ One; Two; Three ]

    static member next repSet =
        match repSet with
        | RepSet.One -> RepSet.Two
        | RepSet.Two -> RepSet.Three
        | RepSet.Three -> RepSet.Complete
        | RepSet.Complete -> invalidOp "Can't advance beyond complete"

type Wave =
    | One
    | Two
    | Three
    | Four

    static member all = [ One; Two; Three; Four ]

    static member next wave =
        match wave with
        | Wave.One -> Wave.Two
        | Wave.Two -> Wave.Three
        | Wave.Three -> Wave.Four
        | Wave.Four -> Wave.One

    static member previous wave =
        match wave with
        | Wave.One -> Wave.Four
        | Wave.Two -> Wave.One
        | Wave.Three -> Wave.Two
        | Wave.Four -> Wave.Three

type WorkoutPlan =
    { Exercise: Exercise
      Sets: Map<Wave * RepSet, Weight * uint> }

    static member zero =
        { Exercise = Exercise.Squats
          Sets = Map.empty }

type WeightIncreases = WeightIncreases of Map<Exercise, Map<MeasurementSystem, Weight>>

type WeightIncreases with
    static member private exerciseMap =
        Exercise.all
        |> List.map (fun e -> (e, [ (Imperial, Weight.zero); (Metric, Weight.zero) ] |> Map.ofList))
        |> Map.ofList

    static member zero = WeightIncreases.exerciseMap |> WeightIncreases.WeightIncreases

    static member standard =
        WeightIncreases.exerciseMap
        |> Map.map (fun exercise _ ->
            (if exercise.isLower then
                 [ (Imperial, 10m |> Weight); (Metric, 5m |> Weight) ]
             else
                 [ (Imperial, 5m |> Weight); (Metric, 2.5m |> Weight) ])
            |> Map.ofList)
        |> WeightIncreases.WeightIncreases

    static member light =
        WeightIncreases.exerciseMap
        |> Map.map (fun exercise _ ->
            (if exercise.isLower then
                 [ (Imperial, 5m |> Weight); (Metric, 2.5m |> Weight) ]
             else
                 [ (Imperial, 2.5m |> Weight); (Metric, 1.25m |> Weight) ])
            |> Map.ofList)
        |> WeightIncreases.WeightIncreases

    member this.calculate(units: MeasurementSystem, exercise: Exercise, weight: Weight) =
        match this with
        | WeightIncreases wi -> wi[exercise][units] + weight

    member this.allWeightsArePositive() =
        if
            not (
                match this with
                | WeightIncreases wi ->
                    wi
                    |> Map.forall (fun _ weights -> weights |> Map.forall (fun _ w -> w > Weight.zero))
            )
        then
            invalidOp "One or more weights was less than or equal to zero"
