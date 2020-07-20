open System

let imgWidth = 256
let imgHeight = 256
let maxIntensity = 255

[<EntryPoint>]
let main argv =
    printfn "P3"
    printfn "%i %i" imgWidth imgHeight
    printfn "%i" maxIntensity
    for j = imgHeight - 1 downto 0 do
        eprintfn "[%i] scan lines remaining" j
        for i = 0 to imgWidth - 1 do
            let fWidth = double(imgWidth)
            let fHeight = double(imgHeight)
            let r = double(maxIntensity) *  (double(i) / (fWidth - 1.0))
            let g = double(maxIntensity) *  (double(j) / (fHeight - 1.0))
            let b = double(maxIntensity) * 0.25
            
            let ir = int(r)
            let ig = int(g)
            let ib = int(b)

            printfn "%i %i %i" ir ig ib
            ()
    eprintfn "Done."
    0 // return an integer exit code
