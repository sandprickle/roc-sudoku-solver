interface Coord
    exposes [
        Coord,
        getRow,
        getCol,
        getBox,
        fromRowCol,
        toRowCol,
        fromXY,
        toXY,
        toU64,
        fromInt,
        increment,
    ]
    imports []

## The coordinate of a cell in a Sudoku grid
Coord := U8

## The row that contains this Coord
getRow : Coord -> U8
getRow = \@Coord index ->
    index // 9

## The column that contains this Coord
getCol : Coord -> U8
getCol = \@Coord index ->
    index % 9

## The box that contains this Coord
getBox : Coord -> U8
getBox = \coord ->
    row = getRow coord
    col = getCol coord

    (row // 3) * 3 + (col // 3)

expect
    List.map
        [
            @Coord 0,
            @Coord 8,
            @Coord 27,
            @Coord 40,
        ]
        getBox
    == [0, 2, 3, 4]

fromRowCol : U8, U8 -> Coord
fromRowCol = \inRow, inCol ->
    normalize = \n ->
        if (n < 0) then
            0
        else if (n > 8) then
            8
        else
            n
    row = normalize inRow
    col = normalize inCol

    (row * 9) + col |> @Coord

toRowCol : Coord -> { row : U8, col : U8 }
toRowCol = \coord -> {
    row: getRow coord,
    col: getCol coord,
}

fromXY : U8, U8 -> Coord
fromXY = \x, y ->
    fromRowCol y x

toXY : Coord -> { x : U8, y : U8 }
toXY = \index ->
    { row, col } = toRowCol index
    { x: col, y: row }

toU64 : Coord -> U64
toU64 = \@Coord index ->
    Num.toU64 index

fromInt : Int * -> Coord
fromInt = \int ->
    normalize = \n ->
        if (n < 0) then
            0
        else if (n > 80) then
            80
        else
            n
    Num.toU8 (normalize int) |> @Coord

increment : Coord -> Coord
increment = \@Coord coord ->
    if coord < 80 then
        coord + 1 |> @Coord
    else
        @Coord coord
