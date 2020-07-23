module Program

open System
open Types

// Image
let aspectRatio = 16.0 / 9.0
let imgWidth = 400
let imgHeight = int(float(imgWidth) / aspectRatio)
let maxIntensity = 255

let fWidth = double(imgWidth)
let fHeight = double(imgHeight)

// Camera
let viewportHeight = 2.0
let viewportWidth = viewportHeight * aspectRatio
let focalLength = 1.0

let origin = Point3.create 0.0 0.0 0.0
let horizontal = Vec3.create viewportWidth 0.0 0.0
let vertical = Vec3.create 0.0 viewportHeight 0.0
let lowerLeftCorner = origin - (horizontal / 2.0) - (vertical / 2.0) - (Vec3.create 0.0 0.0 focalLength)


[<EntryPoint>]
let main argv =
    printfn "P3"
    printfn "%i %i" imgWidth imgHeight
    printfn "%i" maxIntensity

    for j = imgHeight - 1 downto 0 do
        eprintfn "[%i] scan lines remaining" j
        for i = 0 to imgWidth - 1 do
            let u = double(i) / (fWidth - 1.0)
            let v = double(j) / (fHeight - 1.0)
            let r = Ray.create origin (lowerLeftCorner + (u * horizontal) + (v * vertical) - origin)

            Ray.colour r 
            |> Colour.writeColour
            |> printfn "%s"
            ()
    eprintfn "Done."
    0 // return an integer exit code
