module LifeTests

open Xunit
open FsUnit.Xunit
open Ploeh.AutoFixture

open Life.Game

let fixture = new Fixture()


[<Fact>] 
 let ``anyLiving, when no cells are living, returns false.`` ()=
     (Array2D.create 1 1 false) |> anyLiving |> should be False

[<Fact>]
 let ``anyLiving, when all cells are living, returns true.`` ()=
     (Array2D.create 1 1 true) |> anyLiving |> should be True

[<Fact>]
 let ``anyLiving, when some cells are living, returns true.`` ()=
     (Array2D.init 2 2 (fun x y -> if x = 1 then true else false)) |> anyLiving |> should be True

 
[<Fact>]
 let ``cellAvailable, when the cell is inside bounds returns true.`` ()=
     cellAvailable (Array2D.create 3 3 false) 1 1 |> should be True

[<Theory>]
[<InlineData(-1,1)>]
[<InlineData(1,-1)>]
[<InlineData(10,1)>]
[<InlineData(1,10)>]
let  ``cellAvailable, when the cell is outside bounds returns false.`` (x, y)=
     cellAvailable (Array2D.create 3 3 false) x y |> should be False


 type TestNeighboursHelpers ()=
    member this.rnd = new System.Random()
    member this.rows = this.rnd.Next(20)
    member this.columns = this.rows

    member this.world = Array2D.init this.rows this.columns (fun r c -> if this.rnd.Next(2) = 0 then false else true)

let helper = new TestNeighboursHelpers()

let getCellSafely world row column =
    if ((row >= 0 && row < Array2D.length1 world) &&
          (column >= 0 && column < Array2D.length2 world))
        then world.[row, column]
        else false

let testNeighbours world row column (neighbours:bool list) =
    neighbours.Item 0 = (getCellSafely world (row - 1) (column - 1)) &&
    neighbours.Item 1 = (getCellSafely world (row - 1) column) &&
    neighbours.Item 2 = (getCellSafely world (row - 1) (column + 1)) &&
    neighbours.Item 3 = (getCellSafely world row (column - 1)) &&
    neighbours.Item 4 = (getCellSafely world row column) &&
    neighbours.Item 5 = (getCellSafely world row (column + 1)) &&
    neighbours.Item 6 = (getCellSafely world (row + 1) (column - 1)) &&
    neighbours.Item 7 = (getCellSafely world (row + 1) column) &&
    neighbours.Item 8 = (getCellSafely world (row + 1) (column + 1))

[<Fact>]
 let ``testNeighbours always returns the neighbouring cells`` ()=
     let pivotRow = helper.rnd.Next(helper.rows) in
        let pivotColumn = helper.rnd.Next(helper.columns) in
            let result = neighbours helper.world pivotRow pivotColumn in

                testNeighbours helper.world pivotRow pivotColumn result |> should be True
