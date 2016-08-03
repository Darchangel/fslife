namespace Life

module Game =
    open System

    let anyLiving world =
        world |> Seq.cast<bool> |> Seq.filter (fun x -> x) |> Seq.length > 0

    let cellAvailable world row column =
        row >= 0 && row < (Array2D.length1 world) && column >= 0 && column < (Array2D.length2 world)

        
    let neighbours (world : bool[,]) row column =
        let rows = [row - 1; row; row + 1]
        let columns = [column - 1; column; column + 1]

        [for neighbourRow in rows do
            for neighbourColumn in columns do
                yield if ((neighbourRow <> row && neighbourRow >= 0 && neighbourRow < Array2D.length1 world) &&
                          (neighbourColumn <> column && neighbourColumn >= 0 && neighbourColumn < Array2D.length2 world))
                      then world.[neighbourRow, neighbourColumn]
                      else false]


    let liveNeighbours world row column =
        (List.filter (fun a -> a = true) (neighbours world row column)).Length


    let nextGenCell (world : bool[,]) row column = 
        let cell = world.[row, column]
        let live_neighbours = liveNeighbours world row column
        if cell then

            (live_neighbours = 2 || live_neighbours = 3)
        else
            live_neighbours = 3

    let nextGen world =
        Array2D.init (Array2D.length1 world) (Array2D.length2 world) (fun row column -> nextGenCell world row column)

    let print world =
        Array2D.init (Array2D.length1 world) (Array2D.length2 world) (fun row column -> if world.[row, column] = true then "O" else " ")

    let compare world1 world2 =
        let flatWorld1 = world1 |> Seq.cast<bool>
        let flatWorld2 = world2 |> Seq.cast<bool>

        (Seq.compareWith (fun a b -> if a = b then 0 else 1) flatWorld1 flatWorld2) = 0
    
    let rec gameLoop world gen =
        Console.Clear()
        printfn "Generation %d:" gen
        printfn "%A" (print world)

        if not (anyLiving world) then
            printfn ""
            printfn "Unfortunately, your whole civilization died..."
        else
            System.Threading.Thread.Sleep 500
            let nextWorld = (nextGen world)

            if compare world nextWorld then
                printfn ""
                printfn "Congratulations, your civilization stabilized!"
            else
                gameLoop nextWorld (gen + 1)
        
    let start world =
        gameLoop world 1
        printfn "(press any key to exit)"
        Console.ReadKey() |> ignore

