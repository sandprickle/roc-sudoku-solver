interface Sudoku.GenericGrid
    exposes [
        Grid,
        init,
        get,
        set,
        map,
        findFirstCoord,
        fromList,
        fromListNormalize,
        toRows,
        toCols,
        toBoxes,
        prettyPrint,
        toList,
        isLegal,
        isSolvable,
        numberLegal,
    ]

    imports [
        Sudoku.Number.{ Number },
        Sudoku.Coord.{ Coord },
    ]

## A 9 x 9 Sudoku Grid
Grid a := {
    default : a,
    cells : List a,
}

map : Grid a, (a -> b) -> Grid b
map = \@Grid grid, fn ->
    @Grid {
        default: fn grid.default,
        cells: List.map grid.cells fn,
    }

findFirstCoord : Grid a, (a -> Bool) -> Result Coord [NotFound]
findFirstCoord = \@Grid { cells }, fn ->
    when List.findFirstIndex cells fn is
        Ok index ->
            Ok (Sudoku.Coord.fromInt index)

        Err _ ->
            Err NotFound

# Grid Creation

## Initialize a Grid filling all cells with a default value
init : a -> Grid a
init = \default ->
    @Grid {
        default,
        cells: List.repeat default 81,
    }

## Create a Grid from a List of values.
## Returns (Ok _) if the input list is of length 81, otherwise returns an error.
fromList : List a, a -> Result (Grid a) [ListTooLong, ListTooShort]
fromList = \inputList, default ->
    if (List.len inputList) > 81 then
        Err ListTooLong
    else if (List.len inputList) < 81 then
        Err ListTooShort
    else
        @Grid {
            default,
            cells: inputList,
        }
        |> Ok

## Create a Grid from a List of values.
## Pad or truncate the list if it is not the correct length.
fromListNormalize : List a, a -> Grid a
fromListNormalize = \inputList, default ->
    list =
        if List.len inputList > 81 then
            List.takeFirst inputList 81
        else if List.len inputList < 81 then
            List.concat
                inputList
                (List.repeat default (81 - List.len inputList))
        else
            inputList

    @Grid {
        default,
        cells: list,
    }

toList : Grid a -> List a
toList = \@Grid grid ->
    grid.cells

get : Grid a, Coord -> a
get = \@Grid { default, cells }, coord ->
    cells
    |> List.get (Sudoku.Coord.toNat coord)
    |> Result.withDefault default

set : Grid a, Coord, a -> Grid a
set = \@Grid { default, cells }, coord, value ->
    @Grid {
        default,
        cells: cells
        |> List.set (Sudoku.Coord.toNat coord) value,
    }

houseRange = { start: At 0, end: At 8 }

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

getRow : Grid a, U8 -> List a
getRow = \grid, rowNum ->
    List.map
        (rowCoords rowNum)
        (\coord -> get grid coord)

getCol : Grid a, U8 -> List a
getCol = \grid, colNum ->
    List.map
        (colCoords colNum)
        (\coord -> get grid coord)

getBox : Grid a, U8 -> List a
getBox = \grid, boxNum ->
    List.map
        (boxCoords boxNum)
        (\coord -> get grid coord)

toRows : Grid a -> List (List a)
toRows = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getRow grid n)

toCols : Grid a -> List (List a)
toCols = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getCol grid n)

toBoxes : Grid a -> List (List a)
toBoxes = \grid ->
    List.map
        (List.range houseRange)
        (\n -> getBox grid n)

# Display

prettyPrint : Grid a, (a -> Str) -> Str
prettyPrint = \grid, viewCell ->
    templates = {
        lineTop: "┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓\n",
        lineMidThin: "┠───┼───┼───╂───┼───┼───╂───┼───┼───┨\n",
        lineMidThick: "┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫\n",
        lineBottom: "┗━━━┷━━━┷━━━┻━━━┷━━━┷━━━┻━━━┷━━━┷━━━┛",
        row: "┃ _ │ _ │ _ ┃ _ │ _ │ _ ┃ _ │ _ │ _ ┃\n",
    }

    formatRow : List Str -> Str
    formatRow = \row ->
        List.walk
            row
            templates.row
            (\template, cell ->
                Str.replaceFirst template "_" cell
            )

    rows =
        grid
        |> map viewCell
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

# Sudoku Logic

## Determine whether a Grid is solvable, i.e. whether it has at least 17 clues.
isSolvable : Grid a, (a -> [Given, Empty]) -> Bool
isSolvable = \@Grid grid, cellValue ->
    17
    <= grid.cells
    |> List.countIf
        (\cell ->
            when cellValue cell is
                Empty -> Bool.false
                Given -> Bool.true)

## A cell representation for use in legality checking functions
BasicCell : [Fixed Number, Empty]

## Determine whether a house (row, column, or box) is legal
## i.e. whether it contains no duplicate numbers
houseOk : List BasicCell -> Bool
houseOk = \house ->
    house
    |> List.keepOks
        (\cell ->
            when cell is
                Fixed num -> Ok num
                Empty -> Err {})
    |> allUnique

## Determine whether a collection of rows, columns, or boxes is legal
housesOk : List (List BasicCell) -> Bool
housesOk = \houses ->
    # TODO: (Perf) Use List.walkUntil to break early if a house is not legal
    houses
    |> List.map houseOk
    |> List.all identity

## Determine whether a Grid is legal
isLegal : Grid a, (a -> BasicCell) -> Bool
isLegal = \inputGrid, toBasicCell ->
    grid = map inputGrid toBasicCell
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

numberLegal : Grid a, Coord, Number, (a -> BasicCell) -> Bool
numberLegal = \inputGrid, coord, num, toBasicCell ->
    grid = map inputGrid toBasicCell
    cell = get grid coord
    when cell is
        Fixed _ -> Bool.false
        Empty ->
            newGrid = set grid coord (Fixed num)
            row = getRow newGrid (Sudoku.Coord.getRow coord)
            col = getCol newGrid (Sudoku.Coord.getCol coord)
            box = getBox newGrid (Sudoku.Coord.getBox coord)

            housesOk [row, col, box]

# Helpers

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

