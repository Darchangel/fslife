// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Life.Game

[<EntryPoint>]
let main argv = 
    let rows = 10
    let columns = 10
    let rnd = new System.Random()

    let world = Array2D.init rows columns (fun r c -> if rnd.Next(2) = 0 then false else true)

    start world

    0 // return an integer exit code

