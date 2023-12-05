interface Sudoku.Solve
    exposes [
        Puzzle,
        puzzleFromStr,
        prettyPrint,
        isLegal,
        isSolvable,
    ]
    imports [
        Sudoku.Number.{ Number },
        Sudoku.Grid.{ Grid, Cell },
        Sudoku.Coord.{ Coord },
    ]

Puzzle : Grid

isSolvable : Puzzle -> Bool
isSolvable =
    Sudoku.Grid.isSolvable

isLegal : Puzzle -> Bool
isLegal =
    Sudoku.Grid.isLegal

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
                            Err _ -> Empty Sudoku.Number.fullSet
                    ))
        |> List.join

    Sudoku.Grid.fromListNormalize cellList

prettyPrint : Puzzle -> Str
prettyPrint = \puzzle ->
    Sudoku.Grid.prettyPrint
        puzzle

testPuzzle1 : Puzzle
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

testPuzzle2 : Puzzle
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
    |> puzzleFromStr

expect testPuzzle2 |> isSolvable == Bool.false
expect testPuzzle2 |> isLegal == Bool.false

