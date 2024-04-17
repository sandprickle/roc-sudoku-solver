interface GenericGrid
    exposes [
        Grid,
        init,
        fromCsvStr,
    ]

    imports []

## A 9 x 9 Sudoku Grid
Grid cell := List cell implements [Eq]

init : cell -> Grid cell
init = \defaultCell ->
    List.repeat defaultCell 81 |> @Grid

## Create a Grid from a List of values.
## Pad or truncate the list if it is not the correct length.
fromList : cell -> (List cell -> Grid cell)
fromList = \defaultCell ->
    \inputList ->
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

fromCsvStr : cell, (Str -> cell) -> (Str -> Grid cell)
fromCsvStr = \defaultCell, cellFromStr ->
    \str ->
        str
        |> Str.split "\n"
        |> List.map (\row -> Str.split row ",")
        |> List.map
            (\row -> List.map
                    row
                    cellFromStr
            )
        |> List.join
        |> (fromList defaultCell)

