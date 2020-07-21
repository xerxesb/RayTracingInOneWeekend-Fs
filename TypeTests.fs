module TypeTests

open Types

let verify checkStr condition =
    let esc = string (char 0x1B)
    if condition then
        System.Console.WriteLine(checkStr + " [" + esc + "[32;1m" + condition.ToString () + esc + "[0m" + "]")
    else
        System.Console.WriteLine(checkStr + " [" + esc + "[31;1m" + condition.ToString () + esc + "[0m" + "]")

let verifyEquality checkStr a b =
    let esc = string (char 0x1B)
    if a = b then
        System.Console.WriteLine(checkStr + ": (" + a.ToString () + " = " + b.ToString () + ") [" + esc + "[32;1m" + (a = b).ToString () + esc + "[0m" + "]")
    else
        System.Console.WriteLine(checkStr + " " + esc + "[31;1m" + "Expected [" + a.ToString () + "] but was [" + b.ToString () + "]" + esc + "[0m")


printfn "\n----------------------------------"
printfn "Artisinal testing for Types module"
printfn "----------------------------------"

let a = { X = 1.0 ; Y = 2.0 ; Z = 3.0 }
let b = Vec3.create 1.0 2.0 3.0

verify "a = b should be True" (a = b)

let c = -a
verify "c.X should be -1.0" (c.X = -1.0)
verify "c.Y should be -2.0" (c.Y = -2.0)
verify "c.Z should be -3.0" (c.Z = -3.0)

let ax = a.[0]
let ay = a.[1]
let az = a.[2]
verify "a.[0] should be 1.0" (ax = 1.0)
verify "a.[1] should be 2.0" (ay = 2.0)
verify "a.[2] should be 3.0" (az = 3.0)

let d = a + b
verify "(a + b) d.X should be 2.0" (d.X = 2.0)
verify "(a + b) d.Y should be 4.0" (d.Y = 4.0)
verify "(a + b) d.Z should be 6.0" (d.Z = 6.0)

let d2 = d - b
verify "(d - b) d2 should be a" (d2 = a)

let e = a * 4.0
verify "(a * 4) e.X should be 4.0" (e.X = 4.0)
verify "(a * 4) e.Y should be 8.0" (e.Y = 8.0)
verify "(a * 4) e.Z should be 12.0" (e.Z = 12.0)

let e2 = a * a
verify "(a * a) e2.X should be 1.0" (e2.X = 1.0)
verify "(a * a) e2.Y should be 4.0" (e2.Y = 4.0)
verify "(a * a) e2.Z should be 9.0" (e2.Z = 9.0)

let f = e / 4.0
verify "(e / 4) f.X should be 4.0" (e.X = 4.0)
verify "(e / 4) f.Y should be 8.0" (e.Y = 8.0)
verify "(e / 4) f.Z should be 12.0" (e.Z = 12.0)

let lsq = Vec3.lengthSq a
let len = Vec3.length a
verify "(lengthSq a) lsq should be 14.0" (lsq = 14.0)
verify "(length a) len should be sqrt(14.0)" (len = sqrt(14.0))

let dot = Vec3.dot a b
verify "(dot a b) dot should be 14.0" (dot = 14.0)

let cross = Vec3.cross (Vec3.create 12.0 2.0 4.0) (Vec3.create 22.0 44.0 66.0)
verifyEquality "(cross a f) cross.X should be -44" -44.0 cross.X
verifyEquality "(cross a f) cross.Y should be -704" -704.0 cross.Y
verifyEquality "(cross a f) cross.Z should be 484" 484.0 cross.Z

let uv = Vec3.unitVector a 
verifyEquality "(unitVector a) uv should be { 1; 2; 3 }" {X = 1.0; Y = 2.0; Z = 3.0} a

let c1 = Colour.create 4.0 5.0 6.0
verifyEquality "(colour) c1 should be { 4 ; 5; 6 }" { X = 4.0; Y = 5.0; Z = 6.0 } c1

let colourStr = Colour.writeColour c1
verifyEquality "(writeColour) colourStr should be '1024.00 1279.99 1535.99'" "1024.00 1279.99 1535.99" colourStr

exit 0