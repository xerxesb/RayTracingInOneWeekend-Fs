﻿module Program

open System
open Types

// Image
let aspectRatio = 16.0 / 9.0
let imgWidth = 640
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

let pointColour (j, i) =
    let u = double(i) / (fWidth - 1.0)
    let v = double(j) / (fHeight - 1.0)
    let r = Ray.create origin (lowerLeftCorner + (u * horizontal) + (v * vertical) - origin)
    Ray.colour r 

[<EntryPoint>]
let main argv =
    eprintfn "Rendering..."
    let start = DateTime.Now

    printfn "P3"
    printfn "%i %i" imgWidth imgHeight
    printfn "%i" maxIntensity

    Seq.allPairs [imgHeight-1..-1..0] [0..imgWidth-1]
    |> Seq.toArray
    |> Array.Parallel.map (pointColour >> Colour.writeColour)
    |> Array.iter (printfn "%s")

    let finish = DateTime.Now
    eprintfn "Done. [%f seconds]" (finish-start).TotalSeconds 
    0 // return an integer exit code
