interface Grid
    exposes [
        Grid,
        Cell,
        init,
        fromList,
        fromStr,
        get,
        set,
        map,
        findFirst,
        findFirstCoord,
        getRow,
        getCol,
        getBox,
        toRows,
        toCols,
        toBoxes,
        sufficientHints,
        isLegal,
        numberIsLegal,
        prune,
        prettyPrint,
        possibilities,
    ]

    imports [
        Number.{ Number },
        Coord.{ Coord },
    ]

Grid := List Cell implements [Eq]

Cell : [
    Empty (List Number),
    Fixed Number,
]

defaultCell : Cell
defaultCell = Empty Number.all

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
                        when Number.fromStr numStr is
                            Ok num -> Fixed num
                            Err _ -> defaultCell
                    ))
        |> List.join

    fromList list

get : Grid, Coord -> Cell
get = \@Grid cells, coord ->
    cells
    |> List.get (Coord.toU64 coord)
    |> Result.withDefault defaultCell

set : Grid, Coord, Cell -> Grid
set = \@Grid cells, coord, value ->
    cells
    |> List.set (Coord.toU64 coord) value
    |> @Grid

map : Grid, (Cell -> Cell) -> Grid
map = \@Grid cells, fn ->
    cells
    |> List.map fn
    |> @Grid

possibilities : Grid -> List (List Number)
possibilities = \@Grid cells ->
    List.keepOks
        cells
        (\cell ->
            when cell is
                Fixed _ -> Err NotEmpty
                Empty nums -> Ok nums)

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

findFirst : Grid, (Cell -> Bool) -> Result Cell [NotFound]
findFirst = \@Grid cells, fn ->
    List.findFirst cells fn

findFirstCoord : Grid, (Cell -> Bool) -> Result Coord [NotFound]
findFirstCoord = \@Grid cells, fn ->
    List.findFirstIndex cells fn |> Result.map Coord.fromInt

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
                (\col -> List.get col (Num.toU64 n)
                    |> Result.withDefault defaultCell))
    |> List.join
    |> @Grid

toBoxes : Grid -> List (List Cell)
toBoxes = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getBox grid n)

# Sudoku Logic

## Determine whether a Grid has at least 17 clues
sufficientHints : Grid -> Bool
sufficientHints = \@Grid cells ->
    filledCells = List.countIf
        cells
        (\cell ->
            when cell is
                Empty _ -> Bool.false
                Fixed _ -> Bool.true

        )

    filledCells >= 17

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
isLegal : Grid -> Bool
isLegal = \grid ->
    rowsOk =
        toRows grid
        |> housesOk

    colsOk =
        toCols grid
        |> housesOk

    boxesOk =
        toBoxes grid
        |> housesOk

    rowsOk && colsOk && boxesOk

numberIsLegal : Grid, Coord, Number -> Bool
numberIsLegal = \grid, coord, num ->
    cell = get grid coord
    when cell is
        Fixed _ -> Bool.false
        Empty _ ->
            newGrid = set grid coord (Fixed num)
            row = getRow newGrid (Coord.getRow coord)
            col = getCol newGrid (Coord.getCol coord)
            box = getBox newGrid (Coord.getBox coord)

            housesOk [row, col, box]

prune : Grid -> Grid
prune = \grid ->
    newGrid =
        grid
        |> mapRows pruneHouse
        |> mapCols pruneHouse
        |> mapBoxes pruneHouse

    if newGrid == grid then
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
                            |> Result.withDefault Number.one
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
            Number.one,
            Number.two,
            Number.three,
            Number.four,
        ],
        Fixed Number.three,
        Empty [Number.four],
    ]
    == [
        Empty [Number.one, Number.two],
        Fixed Number.three,
        Fixed Number.four,
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
                    Fixed num -> Number.toStr num
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

houseRange = { start: At 0, end: At 8 }

rowCoords : U8 -> List Coord
rowCoords = \rowNum ->
    getCoord = \colNum ->
        Coord.fromRowCol rowNum colNum

    List.map
        (List.range houseRange)
        getCoord

colCoords : U8 -> List Coord
colCoords = \colNum ->
    getCoord = \rowNum ->
        Coord.fromRowCol rowNum colNum

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
            (\xCoord -> Coord.fromXY xCoord yCoord)

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

expect testPuzzle1 |> sufficientHints == Bool.true
expect testPuzzle1 |> isLegal == Bool.true
expect testPuzzle1 == testPuzzle1
expect
    testPuzzle1
    |> getCol 0
    ==
    [
        Empty Number.all,
        Fixed Number.two,
        Fixed Number.three,
        Fixed Number.five,
        Empty Number.all,
        Fixed Number.four,
        Empty Number.all,
        Empty Number.all,
        Empty Number.all,

    ]
expect
    testPuzzle1
    |> toBoxes
    |> List.get 0
    == Ok [
        Empty Number.all,
        Fixed Number.nine,
        Empty Number.all,
        Fixed Number.two,
        Empty Number.all,
        Fixed Number.one,
        Fixed Number.three,
        Empty Number.all,
        Fixed Number.five,
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

expect testPuzzle2 |> sufficientHints == Bool.false
expect testPuzzle2 |> isLegal == Bool.false
