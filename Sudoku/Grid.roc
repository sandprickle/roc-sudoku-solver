interface Sudoku.Grid
    exposes [
        Grid,
        Cell,
        init,
        fromList,
        fromListNormalize,
        get,
        set,
        getRow,
        getCol,
        getBox,
        toRows,
        toCols,
        toBoxes,
        isSolvable,
        isLegal,
        numberIsLegal,
        prettyPrint,
    ]

    imports [
        Sudoku.Number.{ Number },
        Sudoku.Coord.{ Coord },
    ]

Grid := List Cell

Cell : [
    Empty (Set Number),
    Fixed Number,
]

defaultCell : Cell
defaultCell = Empty Sudoku.Number.fullSet

init : Grid
init = List.repeat defaultCell 81 |> @Grid

## Create a Grid from a List of values.
## Returns (Ok _) if the input list is of length 81, otherwise returns an error.
fromList : List Cell -> Result Grid [ListTooLong, ListTooShort]
fromList = \inputList ->
    if (List.len inputList) > 81 then
        Err ListTooLong
    else if (List.len inputList) < 81 then
        Err ListTooShort
    else
        @Grid inputList
        |> Ok
# Create a Grid from a List of values.
## Pad or truncate the list if it is not the correct length.
fromListNormalize : List Cell -> Grid
fromListNormalize = \inputList ->
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

toBoxes : Grid -> List (List Cell)
toBoxes = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getBox grid n)

# Sudoku Logic

## Determine whether a Grid is solvable, i.e. whether it has at least 17 clues.
isSolvable : Grid -> Bool
isSolvable = \@Grid cells ->
    17
    <= cells
    |> List.countIf
        (\cell ->
            when cell is
                Empty _ -> Bool.false
                Fixed _ -> Bool.true)

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
            row = getRow newGrid (Sudoku.Coord.getRow coord)
            col = getCol newGrid (Sudoku.Coord.getCol coord)
            box = getBox newGrid (Sudoku.Coord.getBox coord)

            housesOk [row, col, box]

# prune : Grid -> Grid
# prune = \grid ->
#     crash "todo"

# pruneHouse : List Cell -> List Cell
# pruneHouse = \house ->
#     fixedNumbers =
#         house
#         |> List.keepOks
#             (\cell ->
#                 when cell is
#                     Fixed num -> Ok num
#                     Empty _ -> Err {})
#     fixedNumbers
#         |> List.walk house (\house, number ->
#             )
#     crash "wip"

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
