module TypeTests

open Types
open Expecto

[<Tests>]
let VectorTests =
    testList "Vec3 Tests" [
        testCase "Creating a Vec3 with 3 members (X, Y, Z)" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let b = Vec3.create 1.0 2.0 3.0

            Expect.equal a b "a = b should be True"

        testCase "Unary negation of Vec3" <| fun _ -> 
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let c = -a
            Expect.equal c.X -1.0 "c.X should be -1.0"
            Expect.equal c.Y -2.0 "c.Y should be -2.0"
            Expect.equal c.Z -3.0 "c.Z should be -3.0"
        
        testCase "Indexing into Vec3" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let ax = a.[0]
            let ay = a.[1]
            let az = a.[2]
            Expect.equal ax 1.0 "a.[0] should be 1.0"
            Expect.equal ay 2.0 "a.[1] should be 2.0"
            Expect.equal az 3.0 "a.[2] should be 3.0"

        testCase "Adding two Vec3s" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let b = Vec3.create 1.0 2.0 3.0
            let d = a + b
            Expect.equal d.X 2.0 "(a + b) d.X should be 2.0"
            Expect.equal d.Y 4.0 "(a + b) d.Y should be 4.0"
            Expect.equal d.Z 6.0 "(a + b) d.Z should be 6.0"

        testCase "Subtracting two Vec3s" <| fun _ -> 
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let b = Vec3.create 1.0 2.0 3.0
            let d = a + b
            let d2 = d - b
            Expect.equal d2 a "(d - b) d2 should be a" 
        
        testCase "Multiplying two Vec3s" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let e2 = a * a
            Expect.equal e2.X 1.0 "(a * a) e2.X should be 1.0"
            Expect.equal e2.Y 4.0 "(a * a) e2.Y should be 4.0"
            Expect.equal e2.Z 9.0 "(a * a) e2.Z should be 9.0"

        testCase "Multiplying Vec3 by a factor" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let e = a * 4.0
            Expect.equal e.X 4.0 "(a * 4) e.X should be 4.0" 
            Expect.equal e.Y 8.0 "(a * 4) e.Y should be 8.0"
            Expect.equal e.Z 12.0 "(a * 4) e.Z should be 12.0"

        testCase "Dividing Vec3 by a factor" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let e = a * 4.0
            let f = e / 4.0
            Expect.equal e.X 4.0 "(e / 4) f.X should be 4.0"
            Expect.equal e.Y 8.0 "(e / 4) f.Y should be 8.0"
            Expect.equal e.Z 12.0 "(e / 4) f.Z should be 12.0"

        testCase "Length of Vec3" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let lsq = Vec3.lengthSq a
            let len = Vec3.length a
            Expect.equal lsq 14.0 "(lengthSq a) lsq should be 14.0"
            Expect.equal len (sqrt 14.0) "(length a) len should be sqrt(14.0)"

        testCase "Dot product of Vec3s" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let b = Vec3.create 1.0 2.0 3.0
            let dot = Vec3.dot a b
            Expect.equal dot 14.0 "(dot a b) dot should be 14.0"

        testCase "Cross product of Vec3s" <| fun _ ->
            let cross = Vec3.cross (Vec3.create 12.0 2.0 4.0) (Vec3.create 22.0 44.0 66.0)
            Expect.equal cross.X -44.0 "(cross a f) cross.X should be -44"
            Expect.equal cross.Y -704.0 "(cross a f) cross.Y should be -704"
            Expect.equal cross.Z 484.0 "(cross a f) cross.Z should be 484"
        
        testCase "UnitVector of Vec3" <| fun _ -> 
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let uv = Vec3.unitVector a 
            Expect.floatClose Accuracy.high uv.X 0.2672612419 "(unitVector a) uv.X should be as expected"
            Expect.floatClose Accuracy.high uv.Y 0.5345224838 "(unitVector a) uv.Y should be as expected"
            Expect.floatClose Accuracy.high uv.Z 0.8017837257 "(unitVector a) uv.Z should be as expected"
    ]


[<Tests>]
let ColourTests =
    testList "Colour Test" [
        testCase "Can create a Colour" <| fun _ ->
            let c1 = Colour.create 4.0 5.0 6.0
            Expect.equal c1 {X = 4.0; Y = 5.0; Z = 6.0} "(colour) c1 should be { 4 ; 5; 6 }"
        
        testCase "Can write colour string" <| fun _ ->
            let c1 = Colour.create 4.0 5.0 6.0
            let colourStr = Colour.writeColour c1
            Expect.equal colourStr "1020 1275 1530" "(writeColour) colourStr should be '1020 1275 1530'"
    ]


[<Tests>]
let Point3Tests =
    testList "Point3 Tests" [
        testCase "Can create point" <| fun _ ->
            let p1 = Point3.create 1.0 2.0 3.0
            Expect.equal p1 { X = 1.0 ; Y = 2.0 ; Z = 3.0} "(Point3 create) p1 should be { 1; 2; 3 }" 
    ]


[<Tests>]
let RayTests =
    testList "Ray Tests" [
        testCase "Can create a ray from a point and vector" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let p1 = Point3.create 1.0 2.0 3.0
            let r1 = Ray.create p1 a
            Expect.equal r1 (Ray (p1, a)) "(Ray create) r1 should be { p1 * a }"
            Expect.equal r1.Origin p1 "(Ray.origin) r1.origin should be p1"
            Expect.equal r1.Direction a "(Ray.direction) r1.direction should be a"

        testCase "Find the intersecting point of a Ray at distance" <| fun _ ->
            let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
            let p1 = Point3.create 1.0 2.0 3.0
            let r1 = Ray.create p1 a
            let pointOnR1 = Ray.At r1 4.0
            Expect.equal (Point3.create 5.0 10.0 15.0) pointOnR1 "(Ray.at) pointOnR1 should be { 5; 10; 15 }"
    ]
