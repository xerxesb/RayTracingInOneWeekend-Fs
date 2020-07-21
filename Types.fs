module Types

// According to this, we may get more performance if we switch from records to structs
// https://theburningmonk.com/2011/10/fsharp-performance-test-structs-vs-records/
//
type Vec3 = {
        X: double
        Y: double
        Z: double
    } with

    override this.ToString () =
        sprintf "%f %f %f" this.X this.Y this.Z

    /// Negate current Vec3.
    static member (~-) (v: Vec3) =
        { X = -v.X; Y = -v.Y; Z = -v.Z }

    /// Summation of 2x Vec3 values.
    static member (+) (v1: Vec3, v2: Vec3) =
        { X = v1.X + v2.X ; Y = v1.Y + v2.Y ; Z = v1.Z + v2.Z }
    
    /// Difference of 2x Vec3 values.
    static member (-) (v1: Vec3, v2: Vec3) =
        { X = v1.X - v2.X ; Y = v1.Y - v2.Y ; Z = v1.Z - v2.Z }

    /// Product of 2x Vec3 values
    static member ( * ) (v1: Vec3, v2: Vec3) =
        { X = v1.X * v2.X ; Y = v1.Y * v2.Y ; Z = v1.Z * v2.Z }
    
    /// Multiply all points of a Vec3 by an amount
    static member ( * ) (v1: Vec3, t: double) =
        { X = v1.X * t ; Y = v1.Y * t ; Z = v1.Z * t }
    
    /// Divide all points of a Vec3 by an amount
    static member (/) (v1: Vec3, t:double) =
        v1 * (1.0 / t)

    /// Index into the Vec3 by array index
    /// a.[0] = X
    /// a.[1] = Y
    /// a.[2] = Z
    /// a.[>2] = error
    /// 
    /// N.B. this interface could be improved by type modelling to F# types that 
    /// don't permit out-of-bounds access
    member this.Item 
        with get(index) =
            match index with
                | 0 -> this.X
                | 1 -> this.Y
                | 2 -> this.Z
                | _ -> System.Exception "Invalid index into Vector3" |> raise


type Point3 = Vec3
type Colour3 = Vec3     // This could be improved...

module Vec3 =
    /// Create a new Vec3, from 3-point coordinate
    let create x y z =
        { X = x; Y = y; Z = z }

    /// The length of the square of each point of the Vec3
    let lengthSq v =
        v.X ** 2.0 + v.Y ** 2.0 + v.Z ** 2.0

    /// The sqrt of the lengthSq
    let length v =
        sqrt(lengthSq(v))

    /// The dot-product of two Vec3 values
    let dot v1 v2 =
          v1.X * v2.X 
        + v1.Y * v2.Y
        + v1.Z * v2.Z

    /// The cross-product of two Vec3 values
    let cross v1 v2 =
        { X = v1.Y * v2.Z - v1.Z * v2.Y;
          Y = v1.Z * v2.X - v1.X * v2.Z;
          Z = v1.X * v2.Y - v1.Y * v2.X }

    /// The unit vector length of a Vec3
    let unitVector (v: Vec3) = 
        v / (length v)