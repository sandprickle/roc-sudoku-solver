interface Sudoku.BasicSolver
    exposes [
        Cell,
        Puzzle,
        puzzleFromStr,
        prettyPrint,
        isLegal,
        isSolvable,
    ]
    imports [
        Sudoku.Number.{ Number },
        Sudoku.Grid.{ Grid },
        Sudoku.Coord.{ Coord },
    ]

Cell : [
    Empty,
    Fixed Number,
]

Puzzle : Grid Cell

isSolvable : Puzzle -> Bool
isSolvable = \puzzle ->
    Sudoku.Grid.isSolvable
        puzzle
        (\cell ->
            when cell is
                Empty -> Empty
                Fixed _ -> Given)

isLegal : Puzzle -> Bool
isLegal = \puzzle ->
    Sudoku.Grid.isLegal
        puzzle
        (\cell ->
            when cell is
                Empty -> Empty
                Fixed num -> Fixed num
        )

puzzleFromStr : Str -> Puzzle
puzzleFromStr = \str ->
    cellList =
        str
        |> Str.split "\n"
        |> List.map (\row -> Str.split row ",")
        |> List.map
            (\row -> List.map
                    row
                    (\numStr ->
                        when Sudoku.Number.fromStr numStr is
                            Ok num -> Fixed num
                            Err _ -> Empty
                    ))
        |> List.join

    Sudoku.Grid.fromListNormalize cellList Empty

prettyPrint = \puzzle ->
    Sudoku.Grid.prettyPrint
        puzzle
        (\cell ->
            when cell is
                Empty -> " "
                Fixed num -> Sudoku.Number.toStr num)

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
    |> puzzleFromStr
expect testPuzzle1 |> isSolvable == Bool.true
expect testPuzzle1 |> isLegal == Bool.true

testPuzzle2 =
    """
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    ,,,,,,,,
    """
expect testPuzzle2 |> puzzleFromStr |> isSolvable == Bool.false
expect testPuzzle2 |> puzzleFromStr |> isLegal == Bool.true

## Solve a sudoku puzzle using a naive backtracking algorithm
solve : Puzzle -> Result Puzzle _
solve = \puzzle ->
    if !(isSolvable puzzle) then
        Err Unsolvable
    else
        when Sudoku.Grid.findFirstCoord puzzle (\cell -> cell == Empty) is
            Ok coord ->
                when tryNumber puzzle coord Sudoku.Number.one is
                    Ok newPuzzle ->
                        solve newPuzzle

                    Err _ ->
                        crash "todo"

            Err _ ->
                Ok puzzle

tryNumber : Puzzle, Coord, Number -> Result Puzzle [Illegal]
tryNumber = \puzzle, coord, num ->
    if !(isLegal puzzle) then
        Err Illegal
    else
        Ok puzzle
