namespace Tests.Tests

open Xunit
open FsUnit.Xunit
open Ploeh.AutoFixture

open Life.Game


 type ``Test anyLiving`` ()=

    let fixture = new Fixture()

    [<Fact>] member test.
     ``when no cells are living returns false.`` ()=
         (Array2D.create 1 1 false) |> anyLiving |> should be False

    [<Fact>] member test.
     ``when all cells are living returns true.`` ()=
         (Array2D.create 1 1 true) |> anyLiving |> should be True

    [<Fact>] member test.
     ``when some cells are living returns true.`` ()=
         (Array2D.init 2 2 (fun x y -> if x = 1 then true else false)) |> anyLiving |> should be True

 
 type ``Test cellAvailable`` ()=

    [<Fact>] member test.
     ``when the cell is inside bounds returns true.`` ()=
         cellAvailable (Array2D.create 3 3 false) 1 1 |> should be True

    [<Theory>]
    [<InlineData(-1,1)>]
    [<InlineData(1,-1)>]
    [<InlineData(10,1)>]
    [<InlineData(1,10)>]
    member test. ``when the cell is outside bounds returns false.`` (x, y)=
         cellAvailable (Array2D.create 3 3 false) x y |> should be False


 type ``Test neighbours`` ()=
    let rnd = new System.Random()
    let rows = rnd.Next()
    let columns = rows

    let world = Array2D.init rows columns (fun r c -> if rnd.Next(2) = 0 then false else true)

    member test.
      testNeighbours world row column neighbours =
        (Array2D.get neighbours 0 0) = (Array2D.get world row - 1 column - 1) &&
        (Array2D.get neighbours 0 1) = (Array2D.get world row - 1 column) &&
        (Array2D.get neighbours 0 2) = (Array2D.get world row - 1 column + 1) &&
        (Array2D.get neighbours 1 0) = (Array2D.get world row column - 1) &&
        (Array2D.get neighbours 1 1) = (Array2D.get world row column) &&
        (Array2D.get neighbours 1 2) = (Array2D.get world row column + 1) &&
        (Array2D.get neighbours 2 0) = (Array2D.get world row + 1 column - 1) &&
        (Array2D.get neighbours 2 1) = (Array2D.get world row + 1 column) &&
        (Array2D.get neighbours 2 2) = (Array2D.get world row + 1 column + 1)

    [<Fact>] member test.
     ``always returns the neighbouring cells`` ()=
         let pivotRow = rnd.Next(rows) in
            let pivotColumn = rnd.Next(columns) in
                let result = neighbours world pivotRow pivotColumn in

                    testNeighbours world pivotRow pivotRow result |> should be True
