namespace Jacqued

open System
open Microsoft.FSharp.Collections

[<Struct; CustomComparison; CustomEquality>]
type Weight(value: decimal) =
    // do
    //     if value < 0m then
    //         invalidArg (nameof value) "value cannot be less than 0"

    override this.Equals(other) = value = (other :?> Weight).Value
    override this.GetHashCode() = value.GetHashCode()
    override this.ToString() = value.ToString("F2")

    interface IComparable<Weight> with
        member this.CompareTo(other: Weight) = compare value other.Value

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

type Bar =
    private
    | Bar of Weight

    member this.Weight = let (Bar value) = this in value
    static member Of(weight: Weight) = Bar(weight)

[<Struct; CustomComparison; CustomEquality>]
type PlatePair(weight: Weight) =

    member this.Weight = weight * 2
    member this.WeightOfEach = weight

    override this.Equals(other) = weight = (other :?> PlatePair).Weight
    override this.GetHashCode() = weight.GetHashCode()
    override this.ToString() = weight.ToString()

    interface IComparable<PlatePair> with
        member this.CompareTo(other: PlatePair) = compare weight other.Weight

    interface IComparable with
        member this.CompareTo other =
            compare weight (other :?> PlatePair).Weight

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

    static member next exercise =
        match exercise with
        | Squats -> BenchPress
        | BenchPress -> Deadlifts
        | Deadlifts -> OverheadPress
        | OverheadPress -> Squats

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

    static member all = [ One; Two; Three ]

    static member next repSet =
        match repSet with
        | RepSet.One -> RepSet.Two
        | RepSet.Two -> RepSet.Three
        | _ -> invalidOp ""

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

type WorkoutPlan =
    { Exercise: Exercise
      Sets: Map<Wave * RepSet, Weight * uint> }

    static member zero =
        { Exercise = Exercise.Squats
          Sets = Map.empty }
