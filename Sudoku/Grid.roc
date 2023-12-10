interface Sudoku.Grid
    exposes [
        Grid,
        Cell,
        init,
        fromList,
        fromStr,
        get,
        set,
        map,
        getRow,
        getCol,
        getBox,
        toRows,
        toCols,
        toBoxes,
        solvable,
        legal,
        numberIsLegal,
        prune,
        prettyPrint,
    ]

    imports [
        Sudoku.Number.{ Number },
        Sudoku.Coord.{ Coord },
    ]

Grid := List Cell implements [Eq]

Cell : [
    Empty (List Number),
    Fixed Number,
]

defaultCell : Cell
defaultCell = Empty Sudoku.Number.all

init : Grid
init = List.repeat defaultCell 81 |> @Grid

# Create a Grid from a List of values.
## Pad or truncate the list if it is not the correct length.
fromList : List Cell -> Grid
fromList = \inputList ->
    list =
        if List.len inputList > 81 then
            List.takeFirst inputList 81
        else if List.len inputList < 81 then
            List.concat
                inputList
                (List.repeat defaultCell (81 - List.len inputList))
        else
            inputList

    @Grid list

fromStr : Str -> Grid
fromStr = \str ->
    list =
        str
        |> Str.split "\n"
        |> List.map (\row -> Str.split row ",")
        |> List.map
            (\row -> List.map
                    row
                    (\numStr ->
                        when Sudoku.Number.fromStr numStr is
                            Ok num -> Fixed num
                            Err _ -> defaultCell
                    ))
        |> List.join

    fromList list

get : Grid, Coord -> Cell
get = \@Grid cells, coord ->
    cells
    |> List.get (Sudoku.Coord.toNat coord)
    |> Result.withDefault defaultCell

set : Grid, Coord, Cell -> Grid
set = \@Grid cells, coord, value ->
    cells
    |> List.set (Sudoku.Coord.toNat coord) value
    |> @Grid

map : Grid, (Cell -> Cell) -> Grid
map = \@Grid cells, fn ->
    cells
    |> List.map fn
    |> @Grid

mapRows : Grid, (List Cell -> List Cell) -> Grid
mapRows = \grid, fn ->
    grid
    |> toRows
    |> List.map fn
    |> List.join
    |> @Grid

mapCols : Grid, (List Cell -> List Cell) -> Grid
mapCols = \grid, fn ->
    grid
    |> toCols
    |> List.map fn
    |> fromCols

mapBoxes : Grid, (List Cell -> List Cell) -> Grid
mapBoxes = \inputGrid, fn ->
    List.walk
        (List.range houseRange)
        inputGrid
        (\grid, n ->
            newBox = getBox grid n |> fn
            coords = boxCoords n

            List.walkWithIndex
                coords
                grid
                (\grid2, coord, index ->
                    newCell =
                        List.get newBox index
                        |> Result.withDefault defaultCell

                    set grid2 coord newCell
                )

        )

houseRange = { start: At 0, end: At 8 }

getRow : Grid, U8 -> List Cell
getRow = \grid, rowNum ->
    List.map
        (rowCoords rowNum)
        (\coord -> get grid coord)

getCol : Grid, U8 -> List Cell
getCol = \grid, colNum ->
    List.map
        (colCoords colNum)
        (\coord -> get grid coord)

getBox : Grid, U8 -> List Cell
getBox = \grid, boxNum ->
    List.map
        (boxCoords boxNum)
        (\coord -> get grid coord)

toRows : Grid -> List (List Cell)
toRows = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getRow grid n)

toCols : Grid -> List (List Cell)
toCols = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getCol grid n)

fromCols : List (List Cell) -> Grid
fromCols = \cols ->
    List.map
        (List.range houseRange)
        (\n -> List.map
                cols
                (\col -> List.get col (Num.toNat n)
                    |> Result.withDefault defaultCell))
    |> List.join
    |> @Grid

toBoxes : Grid -> List (List Cell)
toBoxes = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getBox grid n)

# Sudoku Logic

## Determine whether a Grid is solvable, i.e. whether it has at least 17 clues.
solvable : Grid -> [Solvable, NotSolvable]
solvable = \@Grid cells ->
    filledCells = List.countIf
        cells
        (\cell ->
            when cell is
                Empty _ -> Bool.false
                Fixed _ -> Bool.true)

    if filledCells >= 17 then
        Solvable
    else
        NotSolvable

## Determine whether a house (row, column, or box) is legal
## i.e. whether it contains no duplicate numbers
houseOk : List Cell -> Bool
houseOk = \house ->
    house
    |> List.keepOks
        (\cell ->
            when cell is
                Fixed num -> Ok num
                Empty _ -> Err {})
    |> allUnique

## Determine whether a collection of rows, columns, or boxes is legal
housesOk : List (List Cell) -> Bool
housesOk = \houses ->
    # TODO: (Perf) Use List.walkUntil to break early if a house is not legal
    houses
    |> List.map houseOk
    |> List.all identity

## Determine whether a Grid is legal
legal : Grid -> [Legal, Illegal]
legal = \grid ->
    rowsOk =
        toRows grid
        |> housesOk

    colsOk =
        toCols grid
        |> housesOk

    boxesOk =
        toBoxes grid
        |> housesOk

    if rowsOk && colsOk && boxesOk then Legal else Illegal

numberIsLegal : Grid, Coord, Number -> Bool
numberIsLegal = \grid, coord, num ->
    cell = get grid coord
    when cell is
        Fixed _ -> Bool.false
        Empty _ ->
            newGrid = set grid coord (Fixed num)
            row = getRow newGrid (Sudoku.Coord.getRow coord)
            col = getCol newGrid (Sudoku.Coord.getCol coord)
            box = getBox newGrid (Sudoku.Coord.getBox coord)

            housesOk [row, col, box]

prune : Grid -> Grid
prune = \grid ->
    newGrid =
        grid
        |> mapRows pruneHouse
        |> mapCols pruneHouse
        |> mapBoxes pruneHouse

    if newGrid == grid then
        # dbg
        #     when get grid (Sudoku.Coord.fromXY 0 0) is
        #         Empty nums ->
        #             nums
        #             |> List.map Sudoku.Number.toStr
        #             |> Str.joinWith " "

        #         _ -> ""

        grid
    else
        prune newGrid

pruneHouse : List Cell -> List Cell
pruneHouse = \house ->

    fixedNumbers =
        house
        |> List.keepOks
            (\cell ->
                when cell is
                    Fixed num -> Ok num
                    Empty _ -> Err {})

    pruneCell = \cell ->
        when cell is
            Fixed _ -> cell
            Empty numbers ->
                if List.len numbers == 1 then
                    Fixed
                        (
                            List.get numbers 0
                            |> Result.withDefault Sudoku.Number.one
                        )
                else
                    Empty
                        (
                            numbers
                            |> List.dropIf
                                (\n -> fixedNumbers |> List.contains n)
                        )

    newHouse = List.map house (\cell -> pruneCell cell)
    if newHouse == house then
        house
    else
        pruneHouse newHouse

expect
    pruneHouse [
        Empty [
            Sudoku.Number.one,
            Sudoku.Number.two,
            Sudoku.Number.three,
            Sudoku.Number.four,
        ],
        Fixed Sudoku.Number.three,
        Empty [Sudoku.Number.four],
    ]
    == [
        Empty [Sudoku.Number.one, Sudoku.Number.two],
        Fixed Sudoku.Number.three,
        Fixed Sudoku.Number.four,
    ]

# Display

prettyPrint : Grid -> Str
prettyPrint = \grid ->
    templates = {
        lineTop: "┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓\n",
        lineMidThin: "┠───┼───┼───╂───┼───┼───╂───┼───┼───┨\n",
        lineMidThick: "┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫\n",
        lineBottom: "┗━━━┷━━━┷━━━┻━━━┷━━━┷━━━┻━━━┷━━━┷━━━┛",
        row: "┃ _ │ _ │ _ ┃ _ │ _ │ _ ┃ _ │ _ │ _ ┃\n",
    }

    formatRow : List Cell -> Str
    formatRow = \row ->
        row
        |> List.map
            (\cell ->
                when cell is
                    Fixed num -> Sudoku.Number.toStr num
                    Empty _ -> " ")
        |> List.walk
            templates.row
            (\template, cell ->
                Str.replaceFirst template "_" cell
            )
    rows =
        grid
        |> toRows
        |> List.map formatRow

    List.walkWithIndex
        rows
        ""
        (\output, row, index ->
            toAdd =
                if index == 0 then
                    Str.concat templates.lineTop row
                else if index % 3 == 0 then
                    Str.concat templates.lineMidThick row
                else if index == 8 then
                    Str.concat templates.lineMidThin row
                    |> Str.concat templates.lineBottom
                else
                    Str.concat templates.lineMidThin row

            Str.concat output toAdd

        )
# Helpers

rowCoords : U8 -> List Coord
rowCoords = \rowNum ->
    getCoord = \colNum ->
        Sudoku.Coord.fromRowCol rowNum colNum

    List.map
        (List.range houseRange)
        getCoord

colCoords : U8 -> List Coord
colCoords = \colNum ->
    getCoord = \rowNum ->
        Sudoku.Coord.fromRowCol rowNum colNum

    List.map
        (List.range houseRange)
        getCoord

boxCoords : U8 -> List Coord
boxCoords = \boxNum ->
    boxRange = { start: At 0, end: At 2 }

    boxRow =
        boxNum // 3

    boxCol =
        boxNum % 3

    yCoords =
        List.map
            (List.range boxRange)
            (\n -> (3 * boxRow) + n)

    xCoords =
        List.map
            (List.range boxRange)
            (\n -> (3 * boxCol) + n)

    coordsInRow = \yCoord ->
        List.map
            xCoords
            (\xCoord -> Sudoku.Coord.fromXY xCoord yCoord)

    List.map yCoords coordsInRow |> List.join

allUnique : List a -> Bool where a implements Eq
allUnique = \list ->
    if List.len list == 0 then
        Bool.true
    else
        { before, others } = List.split list 1
        when List.get before 0 is
            Ok value ->
                if List.contains others value then
                    Bool.false
                else
                    allUnique others

            Err _ -> Bool.true

expect allUnique [1, 2, 5, 7] == Bool.true
expect allUnique ["hi", "hi"] == Bool.false

identity : a -> a
identity = \a -> a

# Tests

testPuzzle1 : Grid
testPuzzle1 =
    """
    0,9,0,4,0,0,0,0,0
    2,0,1,3,0,0,0,0,0
    3,0,5,0,9,0,8,0,0
    5,0,3,0,4,0,0,0,8
    0,8,0,7,0,6,0,3,0
    4,0,0,0,3,0,0,0,7
    0,0,0,0,2,0,4,0,6
    0,0,0,0,0,9,3,0,0
    0,0,0,0,0,0,0,5,0
    """
    |> fromStr

expect testPuzzle1 |> solvable == Solvable
expect testPuzzle1 |> legal == Legal
expect testPuzzle1 == testPuzzle1
expect
    testPuzzle1
    |> getCol 0
    ==
    [
        Empty Sudoku.Number.all,
        Fixed Sudoku.Number.two,
        Fixed Sudoku.Number.three,
        Fixed Sudoku.Number.five,
        Empty Sudoku.Number.all,
        Fixed Sudoku.Number.four,
        Empty Sudoku.Number.all,
        Empty Sudoku.Number.all,
        Empty Sudoku.Number.all,

    ]
expect
    testPuzzle1
    |> toBoxes
    |> List.get 0
    == Ok [
        Empty Sudoku.Number.all,
        Fixed Sudoku.Number.nine,
        Empty Sudoku.Number.all,
        Fixed Sudoku.Number.two,
        Empty Sudoku.Number.all,
        Fixed Sudoku.Number.one,
        Fixed Sudoku.Number.three,
        Empty Sudoku.Number.all,
        Fixed Sudoku.Number.five,
    ]

testPuzzle2 : Grid
testPuzzle2 =
    """
    ,1,1,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    """
    |> fromStr

expect testPuzzle2 |> solvable == NotSolvable
expect testPuzzle2 |> legal == Illegal
