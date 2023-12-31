interface Solve
    exposes [
        Puzzle,
        puzzleFromStr,
        prettyPrint,
        isLegal,
        isSolvable,
    ]
    imports [
        Number.{ Number },
        Grid.{ Grid, Cell },
        Coord.{ Coord },
    ]

Puzzle : Grid

isSolvable : Puzzle -> Bool
isSolvable =
    Grid.isSolvable

isLegal : Puzzle -> Bool
isLegal =
    Grid.isLegal

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
                        when Number.fromStr numStr is
                            Ok num -> Fixed num
                            Err _ -> Empty Number.fullSet
                    ))
        |> List.join

    Grid.fromListNormalize cellList

prettyPrint : Puzzle -> Str
prettyPrint = \puzzle ->
    Grid.prettyPrint
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

