module Program

open System
open Types

let imgWidth = 256
let imgHeight = 256
let maxIntensity = 255

let fWidth = double(imgWidth)
let fHeight = double(imgHeight)

[<EntryPoint>]
let main argv =
    printfn "P3"
    printfn "%i %i" imgWidth imgHeight
    printfn "%i" maxIntensity

    for j = imgHeight - 1 downto 0 do
        eprintfn "[%i] scan lines remaining" j
        for i = 0 to imgWidth - 1 do
            let r = double(i) / (fWidth - 1.0)
            let g = double(j) / (fHeight - 1.0)
            let b = 0.25
            
            Colour.create r g b
            |> Colour.writeColour
            |> printfn "%s"
            ()
    eprintfn "Done."
    0 // return an integer exit code
