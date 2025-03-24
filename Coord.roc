module [
    Coord,
    get_row,
    get_col,
    get_box,
    from_row_col,
    to_row_col,
    from_xy,
    to_xy,
    to_u8,
    to_u64,
    from_int,
    increment,
    first,
    last,
]

## The coordinate of a cell in a Sudoku grid
Coord := U8 implements [Eq]

## The row that contains this Coord
get_row : Coord -> U8
get_row = |@Coord(index)|
    index // 9

## The column that contains this Coord
get_col : Coord -> U8
get_col = |@Coord(index)|
    index % 9

## The box that contains this Coord
get_box : Coord -> U8
get_box = |coord|
    row = get_row(coord)
    col = get_col(coord)

    (row // 3) * 3 + (col // 3)

expect
    List.map(
        [
            @Coord(0),
            @Coord(8),
            @Coord(27),
            @Coord(40),
        ],
        get_box,
    )
    == [0, 2, 3, 4]

from_row_col : U8, U8 -> Coord
from_row_col = |in_row, in_col|
    normalize = |n|
        if (n < 0) then
            0
        else if (n > 8) then
            8
        else
            n
    row = normalize(in_row)
    col = normalize(in_col)

    (row * 9) + col |> @Coord

to_row_col : Coord -> { row : U8, col : U8 }
to_row_col = |coord| {
    row: get_row(coord),
    col: get_col(coord),
}

from_xy : U8, U8 -> Coord
from_xy = |x, y|
    from_row_col(y, x)

to_xy : Coord -> { x : U8, y : U8 }
to_xy = |index|
    { row, col } = to_row_col(index)
    { x: col, y: row }

to_u8 : Coord -> U8
to_u8 = |@Coord(index)|
    index

to_u64 : Coord -> U64
to_u64 = |@Coord(index)|
    Num.to_u64(index)

from_int : Int * -> Coord
from_int = |int|
    normalize = |n|
        if (n < 0) then
            0
        else if (n > 80) then
            80
        else
            n
    Num.to_u8(normalize(int)) |> @Coord

increment : Coord -> Result Coord [OutOfBounds]
increment = |@Coord(coord)|
    if coord < 80 then
        coord + 1 |> @Coord |> Ok
    else
        Err(OutOfBounds)

first : Coord
first = @Coord(0)

last : Coord
last = @Coord(80)
