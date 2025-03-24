module [
    Grid,
    init,
    from_csv_str,
]

## A 9 x 9 Sudoku Grid
Grid cell := List cell implements [Eq]

init : cell -> Grid cell
init = |default_cell|
    List.repeat(default_cell, 81) |> @Grid

## Create a Grid from a List of values.
## Pad or truncate the list if it is not the correct length.
from_list : cell -> (List cell -> Grid cell)
from_list = |default_cell|
    |input_list|
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

from_csv_str : cell, (Str -> cell) -> (Str -> Grid cell)
from_csv_str = |default_cell, cell_from_str|
    |str|
        str
        |> Str.split("\n")
        |> List.map(|row| Str.split(row, ","))
        |> List.map(
            |row|
                List.map(
                    row,
                    cell_from_str,
                ),
        )
        |> List.join
        |> (from_list(default_cell))

