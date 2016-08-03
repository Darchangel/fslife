namespace Life

module Game =
    let anyLiving world =
        world |> Seq.cast<bool> |> Seq.filter (fun x -> x) |> Seq.length > 0

    let cellAvailable world row column =
        row >= 0 && row < (Array2D.length1 world) && column >= 0 && column < (Array2D.length2 world)

    let neighbours (world : bool[,]) row column =
        let rows = [row - 1; row; row + 1]
        let columns = [column - 1; column; column + 1]
        [for neighbourRow in rows do for neighbourColumn in columns do yield if (neighbourRow <> row || neighbourColumn <> column) then  world.[neighbourRow, neighbourColumn] else false]

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


    let rec gameLoop world gen =
        printfn "Generation %d:" gen
        printfn "%A" world

        if not (anyLiving world) then
            printfn "All ya critters died :'("
        else
            System.Threading.Thread.Sleep 500
            gameLoop (nextGen world) (gen + 1)
        
    let start world =
        gameLoop world 1

