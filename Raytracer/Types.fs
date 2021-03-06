module Types

// According to this, we may get more performance if we switch from records to structs
// https://theburningmonk.com/2011/10/fsharp-performance-test-structs-vs-records/
//
type Vec3 =
    { X: double
      Y: double
      Z: double }

    override this.ToString() = sprintf "%f %f %f" this.X this.Y this.Z

    /// <summary>
    /// Negate current Vec3.
    /// </summary>
    /// <param name="v">The Vec3 value</param>
    /// <returns>A new vector value with all dimensions of the Vec3 negated</returns>
    static member (~-)(v: Vec3) = { X = -v.X; Y = -v.Y; Z = -v.Z }

    /// <summary>
    /// Summation of 2x Vec3 values.
    /// </summary>
    /// <param name="v1">The first Vec3 value</param>
    /// <param name="v2">The second Vec3 value</param>
    /// <returns>A new vector value</returns>
    static member (+)(v1: Vec3, v2: Vec3) =
        { X = v1.X + v2.X
          Y = v1.Y + v2.Y
          Z = v1.Z + v2.Z }

    /// <summary>
    /// Difference of 2x Vec3 values.
    /// </summary>
    /// <param name="v1">The first Vec3 value</param>
    /// <param name="v2">The second Vec3 value</param>
    /// <returns>A new vector value</returns>
    static member (-)(v1: Vec3, v2: Vec3) =
        { X = v1.X - v2.X
          Y = v1.Y - v2.Y
          Z = v1.Z - v2.Z }

    /// <summary>
    /// Product of 2x Vec3 values
    /// </summary>
    /// <param name="v1">The first Vec3 value</param>
    /// <param name="v2">The second Vec3 value</param>
    /// <returns>A new vector value</returns>
    static member (*)(v1: Vec3, v2: Vec3) =
        { X = v1.X * v2.X
          Y = v1.Y * v2.Y
          Z = v1.Z * v2.Z }

    /// <summary>
    /// Multiply all points of a Vec3 by an amount representing time (t)
    /// </summary>
    /// <param name="v1">The Vec3 value</param>
    /// <param name="t">The Time factor (t)</param>
    /// <returns>A new vector value</returns>
    static member (*)(v: Vec3, t: double) =
        { X = v.X * t
          Y = v.Y * t
          Z = v.Z * t }

    /// <summary>
    /// Multiply all points of a Vec3 by an amount representing time (t)
    /// </summary>
    /// <param name="v1">The Vec3 value</param>
    /// <param name="t">The Time factor (t)</param>
    /// <returns>A new vector value</returns>
    static member (*)(t: double, v: Vec3) =
        { X = v.X * t
          Y = v.Y * t
          Z = v.Z * t }

    /// <summary>
    /// Divide all points of a Vec3 by an amount representing time (t)
    /// </summary>
    /// <param name="v1">The Vec3 value</param>
    /// <param name="t">The Time factor (t)</param>
    /// <returns>A new vector value</returns>
    static member (/)(v1: Vec3, t: double) = v1 * (1.0 / t)

    /// <summary>
    /// Index into the Vec3 by array index
    /// a.[0] = X
    /// a.[1] = Y
    /// a.[2] = Z
    /// a.[>2] = error
    ///
    /// N.B. this interface could be improved by type modelling to F# types that
    /// don't permit out-of-bounds access
    /// </summary>
    /// <returns>The X, Y, or Z component of the Vec3</returns>
    member this.Item
        with get (index) =
            match index with
            | 0 -> this.X
            | 1 -> this.Y
            | 2 -> this.Z
            | _ ->
                System.Exception "Invalid index into Vector3"
                |> raise


/// <summary>
/// A represenation of a single point in 3D space
/// </summary>
type Point3 = Vec3

/// <summary>
/// A represenation of a colour in R, G, B representation
/// It's a bit of a hack to re-use the Vec3 as a Colour object. This should be improved
/// </summary>
type Colour = Vec3

/// <summary>
/// A represenation of a ray. It is used to determine what colour is seen along some
/// position along the ray (t)
/// </summary>
type Ray =
    | Ray of Point3 * Vec3

    /// <summary>
    /// Returns the point of origin for this ray
    /// </summary>
    /// <returns>Point3 origin</returns>
    member this.Origin =
        let (Ray (origin, _)) = this
        origin

    /// <summary>
    /// Returns the vector representing the direction for this ray
    /// </summary>
    /// <returns>Vec3 vector</returns>
    member this.Direction =
        let (Ray (_, direction)) = this
        direction

    /// <summary>
    /// Returns the Point3 for the given ray (r) at a particular time (t)
    /// </summary>
    /// <returns>A Point3 representing the position of the ray at time t</returns>
    static member At (r: Ray) (t: double) =
        r.Origin + (t * r.Direction)


module Vec3 =
    /// <summary>
    /// Create a new Vec3, from 3-point coordinate
    /// </summary>
    /// <param name="x">X co-ordinate in 3D space</param>
    /// <param name="y">Y co-ordinate in 3D space</param>
    /// <param name="z">Z co-ordinate in 3D space</param>
    /// <returns>A new Vec3 value</returns>
    let create x y z = { X = x; Y = y; Z = z }

    /// <summary>
    /// The length of the square of each point of the Vec3
    /// </summary>
    /// <param name="v">A Vec3</param>
    /// <returns>The sum of the squares of all dimensions in the Vec3</returns>
    let lengthSq v = v.X ** 2.0 + v.Y ** 2.0 + v.Z ** 2.0

    /// <summary>
    /// The sqrt of the lengthSq
    /// </summary>
    /// <param name="v">A Vec3</param>
    /// <returns>The sqrt of the sum of the squares of all dimensions in the Vec3</returns>
    let length v = sqrt (lengthSq (v))

    /// <summary>
    /// The dot-product of two Vec3 values
    /// </summary>
    /// <param name="v1">First Vec3</param>
    /// <param name="v2">Second Vec3</param>
    /// <returns>Dot product of the two Vec3s</returns>
    let dot v1 v2 = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    /// <summary>
    /// The cross-product of two Vec3 values
    /// </summary>
    /// <param name="v1">First Vec3</param>
    /// <param name="v2">Second Vec3</param>
    /// <returns>Cross product of the two Vec3s</returns>
    let cross v1 v2 =
        { X = v1.Y * v2.Z - v1.Z * v2.Y
          Y = v1.Z * v2.X - v1.X * v2.Z
          Z = v1.X * v2.Y - v1.Y * v2.X }

    /// <summary>
    /// The unit vector length of a Vec3
    /// </summary>
    /// <param name="v">A Vec3 value</param>
    /// <returns>A Vec3 representing the unit length of the vector</returns>
    let unitVector (v: Vec3) = v / (length v)


module Point3 =
    /// <summary>
    /// Creates a new Point value
    /// </summary>
    /// <returns>A new point with the provided dimensions</returns>
    let create x y z: Point3 = { X = x; Y = y; Z = z }


module Colour =
    /// <summary>
    /// Creates a new Colour value
    /// </summary>
    /// <returns>Value of type Colour</returns>
    let create r g b: Colour = { X = r; Y = g; Z = b }

    /// <summary>
    /// Formats a Colour value into a printable string in PPM format
    /// </summary>
    /// <returns>String that you can dump straight to stdout</returns>
    let writeColour (c: Colour) =
        let maxIntensity = 255
        let r = double (maxIntensity) * (c.X)
        let g = double (maxIntensity) * (c.Y)
        let b = double (maxIntensity) * (c.Z)

        sprintf "%i %i %i" (int r) (int g) (int b)


module Ray =
    /// <summary>
    /// Creates a new Ray value
    /// </summary>
    /// <returns>A new point with the provided dimensions</returns>
    let create p v: Ray = Ray(p, v)


    /// <summary>
    /// Determines if a provided ray will hit a sphere located at (centre) with radius (radius)
    /// </summary>
    /// <returns>Double indicating the normal at the point the ray intersected with the sphere</returns>
    let hitSphere centre radius (r: Ray) =
        let oc = r.Origin - centre
        let a = Vec3.lengthSq r.Direction
        let halfB = Vec3.dot oc r.Direction
        let c = (Vec3.lengthSq oc) - (radius * radius)
        let discriminant = halfB * halfB - (a * c)

        if discriminant < 0.0 then -1.0 else (-halfB - sqrt (discriminant)) / a

    /// <summary>
    /// Create a colour for the visualisation of a ray. If the ray misses all objects, it will provide the colour ofthe background.
    /// </summary>
    /// <returns>The colour of the point that the ray intersected with the object</returns>
    let colour (r: Ray): Colour =
        let t = hitSphere (Point3.create 0.0 0.0 -1.0) 0.5 r

        if t > 0.0 then
            let n = Vec3.unitVector ((Ray.At r t) - (Vec3.create 0.0 0.0 -1.0))

            0.5 * (Colour.create (n.X + 1.0) (n.Y + 1.0) (n.Z + 1.0))
        else
            let dir = Vec3.unitVector r.Direction
            let t = 0.5 * (dir.Y + 1.0)

            (1.0 - t)
            * (Colour.create 1.0 1.0 1.0)
            + t * (Colour.create 0.5 0.7 1.0)


/// <summary>
/// A represenation of a hit of a ray on an object
/// </summary>
type HitRecord = 
    { Point: Point3; 
      Normal: Vec3; 
      T: double;
      FrontFace: bool option
    } with
    member this.SetFaceNormal (r:Ray) (outwardNormal:Vec3) =
        let frontFace = Vec3.dot r.Direction outwardNormal < 0.0
        let normal = if frontFace then outwardNormal else -outwardNormal
        { this with Normal = normal; FrontFace = Some frontFace }


/// <summary>
/// An interface to define behaviour for a type that will block/intersect with a Ray
/// (F# doesn't have support for Haskell style typeclasses)
/// </summary>
type IHittable =
    abstract member Hit: r:Ray -> tMin:double -> tMax:double -> hitRecord:HitRecord -> bool * HitRecord option


/// <summary>
/// Definition of a sphere type.
/// 
/// Must implement the IHittable interface to detect intersections with rays
/// </summary>
type Sphere = 
    { Centre: Point3
      Radius: double } with
    member this.Hit r tMin tMax hRec =
        (this :> IHittable).Hit r tMin tMax hRec
    interface IHittable with
        member this.Hit r tMin tMax hRec =
            let oc = r.Origin - this.Centre
            let a = Vec3.lengthSq r.Direction
            let halfB = Vec3.dot oc r.Direction
            let c = (Vec3.lengthSq oc) - (this.Radius * this.Radius)
            let discriminant = halfB * halfB - (a * c)

            if discriminant > 0.0 then
                let root = sqrt discriminant

                let temp = (-halfB - root) / a
                if (temp < tMax && temp > tMin) then
                    let hRecTemp = { hRec with T = temp ; Point = (Ray.At r hRec.T) }
                    let outwardNormal = (hRecTemp.Point - this.Centre) / this.Radius
                    let faceNormalHRec = hRec.SetFaceNormal r outwardNormal
                    (true, Some faceNormalHRec)
                else
                    let temp = (-halfB + root) / a
                    if (temp < tMax && temp > tMin) then
                        let hRecTemp = { hRec with T = temp ; Point = (Ray.At r hRec.T) }
                        let outwardNormal = (hRecTemp.Point - this.Centre) / this.Radius
                        let faceNormalHRec = hRec.SetFaceNormal r outwardNormal
                        (true, Some faceNormalHRec)
                    else
                        (false, None)
            else
                (false, None)

