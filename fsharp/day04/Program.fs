open System
open System.IO
open System.Text.RegularExpressions

let contains ((a0, a1), (b0, b1)) = a0 >= b0 && a1 <= b1 || b0 >= a0 && b1 <= a1
let overlaps ((a0, a1), (b0, b1)) = a1 >= b0 && b1 >= a0

[<EntryPoint>]
let main argv =
    let lines = File.ReadAllLines argv.[0]
    let re = Regex("(\\d+)-(\\d+),(\\d+)-(\\d+)")

    let data =
        lines
        |> Seq.map re.Match
        |> Seq.map (fun m ->
            ((int (m.Groups.[1].Value), int (m.Groups.[2].Value)), (int (m.Groups.[3].Value), int (m.Groups.[4].Value))))

    let part1 = data |> Seq.filter contains |> Seq.length
    let part2 = data |> Seq.filter overlaps |> Seq.length

    printfn "%A" part1
    printfn "%A" part2
    0 // return an integer exit code
