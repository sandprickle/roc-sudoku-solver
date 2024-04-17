interface Solve
    exposes [
        backtrackSimple,
    ]
    imports [
        Grid.{ Grid, Cell },
        Coord.{ Coord },
    ]

backtrackSimple : Grid -> Result Grid [NoSolutionFound, NotLegal, TooFewHints]
backtrackSimple = \puzzle ->
    sufficientHints = Grid.sufficientHints puzzle
    puzzleLegal = Grid.isLegal puzzle

    if sufficientHints then
        if puzzleLegal then
            start =
                Grid.findFirstCoord
                    puzzle
                    (\cell ->
                        when cell is
                            Fixed _ -> Bool.false
                            Empty _ -> Bool.true
                    )
                |> Result.withDefault Coord.first

            backtrackSimpleHelp puzzle start
        else
            Err NotLegal
    else
        Err TooFewHints

backtrackSimpleHelp : Grid, Coord -> Result Grid [NoSolutionFound]
backtrackSimpleHelp = \puzzle, currentCoord ->

    currentCell = Grid.get puzzle currentCoord

    when currentCell is
        Fixed _ ->
            when Coord.increment currentCoord is
                Ok newCoord -> backtrackSimpleHelp puzzle newCoord
                Err _ -> Ok puzzle

        Empty possibleNums ->
            testNumsResult = List.walkUntil
                possibleNums
                (NoSolution puzzle currentCoord)
                (\state, num ->
                    when state is
                        NoSolution grid coord ->
                            if Grid.numberIsLegal grid coord num then
                                newGrid = Grid.set grid coord (Fixed num)
                                when Coord.increment coord is
                                    Ok newCoord ->
                                        when
                                            backtrackSimpleHelp
                                                (Grid.prune newGrid)
                                                newCoord
                                        is
                                            Ok solution ->
                                                Solution solution |> Break

                                            Err _ ->
                                                NoSolution grid currentCoord
                                                |> Continue

                                    Err _ -> Solution newGrid |> Break
                            else
                                NoSolution grid currentCoord |> Continue

                        Solution grid ->
                            Solution grid |> Break)

            when testNumsResult is
                NoSolution _ _ ->
                    Err NoSolutionFound

                Solution grid ->
                    Ok grid

