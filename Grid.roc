module [
    Grid,
    Cell,
    init,
    from_list,
    from_str,
    get,
    set,
    map,
    find_first,
    find_first_coord,
    get_row,
    get_col,
    get_box,
    to_rows,
    to_cols,
    to_boxes,
    sufficient_hints,
    is_legal,
    number_is_legal,
    prune,
    pretty_print,
    possibilities,
]

import Number exposing [Number]
import Coord exposing [Coord]

Grid := List Cell implements [Eq]

Cell : [
    Empty (List Number),
    Fixed Number,
]

default_cell : Cell
default_cell = Empty(Number.all)

init : Grid
init = List.repeat(default_cell, 81) |> @Grid

## Create a Grid from a List of values.
## Pad or truncate the list if it is not the correct length.
from_list : List Cell -> Grid
from_list = |input_list|
    list =
        if List.len(input_list) > 81 then
            List.take_first(input_list, 81)
        else if List.len(input_list) < 81 then
            List.concat(
                input_list,
                List.repeat(default_cell, (81 - List.len(input_list))),
            )
        else
            input_list

    @Grid(list)

from_str : Str -> Grid
from_str = |str|
    str
    |> Str.split_on("\n")
    |> List.map(|row| Str.split_on(row, ","))
    |> List.map(
        |row|
            List.map(
                row,
                |num_str|
                    when Number.from_str(num_str) is
                        Ok(num) -> Fixed(num)
                        Err(_) -> default_cell,
            ),
    )
    |> List.join
    |> from_list

get : Grid, Coord -> Cell
get = |@Grid(cells), coord|
    cells
    |> List.get(Coord.to_u64(coord))
    |> Result.with_default(default_cell)

set : Grid, Coord, Cell -> Grid
set = |@Grid(cells), coord, value|
    cells
    |> List.set(Coord.to_u64(coord), value)
    |> @Grid

map : Grid, (Cell -> Cell) -> Grid
map = |@Grid(cells), fn|
    cells
    |> List.map(fn)
    |> @Grid

possibilities : Grid -> List (List Number)
possibilities = |@Grid(cells)|
    List.keep_oks(
        cells,
        |cell|
            when cell is
                Fixed(_) -> Err(NotEmpty)
                Empty(nums) -> Ok(nums),
    )

map_rows : Grid, (List Cell -> List Cell) -> Grid
map_rows = |grid, fn|
    grid
    |> to_rows
    |> List.map(fn)
    |> List.join
    |> @Grid

map_cols : Grid, (List Cell -> List Cell) -> Grid
map_cols = |grid, fn|
    grid
    |> to_cols
    |> List.map(fn)
    |> from_cols

map_boxes : Grid, (List Cell -> List Cell) -> Grid
map_boxes = |input_grid, fn|
    List.walk(
        List.range(house_range),
        input_grid,
        |grid, n|
            new_box = get_box(grid, n) |> fn
            coords = box_coords(n)

            List.walk_with_index(
                coords,
                grid,
                |grid2, coord, index|
                    new_cell =
                        List.get(new_box, index)
                        |> Result.with_default(default_cell)

                    set(grid2, coord, new_cell),
            ),
    )

find_first : Grid, (Cell -> Bool) -> Result Cell [NotFound]
find_first = |@Grid(cells), fn|
    List.find_first(cells, fn)

find_first_coord : Grid, (Cell -> Bool) -> Result Coord [NotFound]
find_first_coord = |@Grid(cells), fn|
    List.find_first_index(cells, fn) |> Result.map_ok(Coord.from_int)

get_row : Grid, U8 -> List Cell
get_row = |grid, row_num|
    List.map(
        row_coords(row_num),
        |coord| get(grid, coord),
    )

get_col : Grid, U8 -> List Cell
get_col = |grid, col_num|
    List.map(
        col_coords(col_num),
        |coord| get(grid, coord),
    )

get_box : Grid, U8 -> List Cell
get_box = |grid, box_num|
    List.map(
        box_coords(box_num),
        |coord| get(grid, coord),
    )

to_rows : Grid -> List (List Cell)
to_rows = |grid|
    List.map(
        List.range(house_range),
        |n| get_row(grid, n),
    )

to_cols : Grid -> List (List Cell)
to_cols = |grid|
    List.map(
        List.range(house_range),
        |n| get_col(grid, n),
    )

from_cols : List (List Cell) -> Grid
from_cols = |cols|
    List.map(
        List.range(house_range),
        |n|
            List.map(
                cols,
                |col|
                    List.get(col, Num.to_u64(n))
                    |> Result.with_default(default_cell),
            ),
    )
    |> List.join
    |> @Grid

to_boxes : Grid -> List (List Cell)
to_boxes = |grid|
    List.map(
        List.range(house_range),
        |n| get_box(grid, n),
    )

# Sudoku Logic

## Determine whether a Grid has at least 17 clues
sufficient_hints : Grid -> Bool
sufficient_hints = |@Grid(cells)|
    filled_cells = List.count_if(
        cells,
        |cell|
            when cell is
                Empty(_) -> Bool.false
                Fixed(_) -> Bool.true,
    )

    filled_cells >= 17

## Determine whether a house (row, column, or box) is legal
## i.e. whether it contains no duplicate numbers
house_ok : List Cell -> Bool
house_ok = |house|
    house
    |> List.keep_oks(
        |cell|
            when cell is
                Fixed(num) -> Ok(num)
                Empty(_) -> Err({}),
    )
    |> all_unique

## Determine whether a collection of rows, columns, or boxes is legal
houses_ok : List (List Cell) -> Bool
houses_ok = |houses|
    # TODO: (Perf) Use List.walkUntil to break early if a house is not legal
    houses
    |> List.map(house_ok)
    |> List.all(identity)

## Determine whether a Grid is legal
is_legal : Grid -> Bool
is_legal = |grid|
    rows_ok =
        to_rows(grid)
        |> houses_ok

    cols_ok =
        to_cols(grid)
        |> houses_ok

    boxes_ok =
        to_boxes(grid)
        |> houses_ok

    rows_ok and cols_ok and boxes_ok

number_is_legal : Grid, Coord, Number -> Bool
number_is_legal = |grid, coord, num|
    cell = get(grid, coord)
    when cell is
        Fixed(_) -> Bool.false
        Empty(_) ->
            new_grid = set(grid, coord, Fixed(num))
            row = get_row(new_grid, Coord.get_row(coord))
            col = get_col(new_grid, Coord.get_col(coord))
            box = get_box(new_grid, Coord.get_box(coord))

            houses_ok([row, col, box])

prune : Grid -> Grid
prune = |grid|
    new_grid =
        grid
        |> map_rows(prune_house)
        |> map_cols(prune_house)
        |> map_boxes(prune_house)

    if new_grid == grid then
        grid
    else
        prune(new_grid)

prune_house : List Cell -> List Cell
prune_house = |house|

    fixed_numbers =
        house
        |> List.keep_oks(
            |cell|
                when cell is
                    Fixed num -> Ok(num)
                    Empty _ -> Err({}),
        )

    prune_cell = |cell|
        when cell is
            Fixed(_) -> cell
            Empty(numbers) ->
                if List.len(numbers) == 1 then
                    Fixed(
                        (
                            List.get(numbers, 0)
                            |> Result.with_default(Number.one)
                        ),
                    )
                else
                    Empty(
                        (
                            numbers
                            |> List.drop_if(
                                |n| fixed_numbers |> List.contains(n),
                            )
                        ),
                    )

    new_house = List.map(house, |cell| prune_cell(cell))
    if new_house == house then
        house
    else
        prune_house(new_house)

expect
    prune_house(
        [
            Empty(
                [
                    Number.one,
                    Number.two,
                    Number.three,
                    Number.four,
                ],
            ),
            Fixed(Number.three),
            Empty([Number.four]),
        ],
    )
    == [
        Empty([Number.one, Number.two]),
        Fixed(Number.three),
        Fixed(Number.four),
    ]

# Display

pretty_print : Grid -> Str
pretty_print = |grid|
    templates = {
        line_top: "┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓\n",
        line_mid_thin: "┠───┼───┼───╂───┼───┼───╂───┼───┼───┨\n",
        line_mid_thick: "┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫\n",
        line_bottom: "┗━━━┷━━━┷━━━┻━━━┷━━━┷━━━┻━━━┷━━━┷━━━┛",
        row: "┃ _ │ _ │ _ ┃ _ │ _ │ _ ┃ _ │ _ │ _ ┃\n",
    }

    format_row : List Cell -> Str
    format_row = |row|
        row
        |> List.map(
            |cell|
                when cell is
                    Fixed(num) -> Number.to_str(num)
                    Empty(_) -> " ",
        )
        |> List.walk(
            templates.row,
            |template, cell|
                Str.replace_first(template, "_", cell),
        )
    rows =
        grid
        |> to_rows
        |> List.map(format_row)

    List.walk_with_index(
        rows,
        "",
        |output, row, index|
            to_add =
                if index == 0 then
                    Str.concat(templates.line_top, row)
                else if index % 3 == 0 then
                    Str.concat(templates.line_mid_thick, row)
                else if index == 8 then
                    Str.concat(templates.line_mid_thin, row)
                    |> Str.concat(templates.line_bottom)
                else
                    Str.concat(templates.line_mid_thin, row)

            Str.concat(output, to_add),
    )

# Helpers

house_range = { start: At(0), end: At(8) }

row_coords : U8 -> List Coord
row_coords = |row_num|
    get_coord = |col_num|
        Coord.from_row_col(row_num, col_num)

    List.map(
        List.range(house_range),
        get_coord,
    )

col_coords : U8 -> List Coord
col_coords = |col_num|
    get_coord = |row_num|
        Coord.from_row_col(row_num, col_num)

    List.map(
        List.range(house_range),
        get_coord,
    )

box_coords : U8 -> List Coord
box_coords = |box_num|
    box_range = { start: At(0), end: At(2) }

    box_row =
        box_num // 3

    box_col =
        box_num % 3

    y_coords =
        List.map(
            List.range(box_range),
            |n| (3 * box_row) + n,
        )

    x_coords =
        List.map(
            List.range(box_range),
            |n| (3 * box_col) + n,
        )

    coords_in_row = |y_coord|
        List.map(
            x_coords,
            |x_coord| Coord.from_xy(x_coord, y_coord),
        )

    List.map(y_coords, coords_in_row) |> List.join

all_unique : List a -> Bool where a implements Eq
all_unique = |list|
    if List.len(list) == 0 then
        Bool.true
    else
        { before, others } = List.split_at(list, 1)
        when List.get(before, 0) is
            Ok(value) ->
                if List.contains(others, value) then
                    Bool.false
                else
                    all_unique(others)

            Err(_) -> Bool.true

expect all_unique([1, 2, 5, 7]) == Bool.true
expect all_unique(["hi", "hi"]) == Bool.false

identity : a -> a
identity = |a| a

# Tests

test_puzzle1 : Grid
test_puzzle1 =
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
    |> from_str

expect test_puzzle1 |> sufficient_hints == Bool.true
expect test_puzzle1 |> is_legal == Bool.true
expect test_puzzle1 == test_puzzle1
expect
    test_puzzle1
    |> get_col(0)
    ==
    [
        Empty(Number.all),
        Fixed(Number.two),
        Fixed(Number.three),
        Fixed(Number.five),
        Empty(Number.all),
        Fixed(Number.four),
        Empty(Number.all),
        Empty(Number.all),
        Empty(Number.all),
    ]
expect
    test_puzzle1
    |> to_boxes
    |> List.get(0)
    == Ok(
        [
            Empty(Number.all),
            Fixed(Number.nine),
            Empty(Number.all),
            Fixed(Number.two),
            Empty(Number.all),
            Fixed(Number.one),
            Fixed(Number.three),
            Empty(Number.all),
            Fixed(Number.five),
        ],
    )

test_puzzle2 : Grid
test_puzzle2 =
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
    |> from_str

expect test_puzzle2 |> sufficient_hints == Bool.false
expect test_puzzle2 |> is_legal == Bool.false
