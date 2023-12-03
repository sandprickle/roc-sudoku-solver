interface Sudoku.Solve
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
        Sudoku.GenericGrid.{ Grid },
        Sudoku.Coord.{ Coord },
    ]

Cell : [
    Empty (Set Number),
    Fixed Number,
]

defaultCell : Cell
defaultCell = Empty Sudoku.Number.fullSet

Puzzle : Grid Cell

isSolvable : Puzzle -> Bool
isSolvable = \puzzle ->
    Sudoku.GenericGrid.isSolvable
        puzzle
        (\cell ->
            when cell is
                Empty _ -> Empty
                Fixed _ -> Given)

isLegal : Puzzle -> Bool
isLegal = \puzzle ->
    Sudoku.GenericGrid.isLegal
        puzzle
        (\cell ->
            when cell is
                Empty _ -> Empty
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
                            Err _ -> Empty Sudoku.Number.fullSet
                    ))
        |> List.join

    Sudoku.GenericGrid.fromListNormalize cellList defaultCell

prettyPrint : Puzzle -> Str
prettyPrint = \puzzle ->
    Sudoku.GenericGrid.prettyPrint
        puzzle
        (\cell ->
            when cell is
                Empty _ -> " "
                Fixed num -> Sudoku.Number.toStr num)

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

