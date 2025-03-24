module [
    backtrack_simple,
]

import Grid exposing [Grid, Cell]
import Coord exposing [Coord]

backtrack_simple : Grid -> Result Grid [NoSolutionFound, NotLegal, TooFewHints]
backtrack_simple = |puzzle|
    sufficient_hints = Grid.sufficient_hints(puzzle)
    puzzle_legal = Grid.is_legal(puzzle)

    if sufficient_hints then
        if puzzle_legal then
            start =
                Grid.find_first_coord(
                    puzzle,
                    |cell|
                        when cell is
                            Fixed(_) -> Bool.false
                            Empty(_) -> Bool.true,
                )
                |> Result.with_default(Coord.first)

            backtrack_simple_help(puzzle, start)
        else
            Err(NotLegal)
    else
        Err(TooFewHints)

backtrack_simple_help : Grid, Coord -> Result Grid [NoSolutionFound]
backtrack_simple_help = |puzzle, current_coord|

    current_cell = Grid.get(puzzle, current_coord)

    when current_cell is
        Fixed(_) ->
            when Coord.increment(current_coord) is
                Ok(new_coord) -> backtrack_simple_help(puzzle, new_coord)
                Err(_) -> Ok(puzzle)

        Empty(possible_nums) ->
            test_nums_result = List.walk_until(
                possible_nums,
                NoSolution(puzzle, current_coord),
                |state, num|
                    when state is
                        NoSolution(grid, coord) ->
                            if Grid.number_is_legal(grid, coord, num) then
                                new_grid = Grid.set(grid, coord, Fixed(num))
                                when Coord.increment(coord) is
                                    Ok(new_coord) ->
                                        when
                                            backtrack_simple_help(
                                                Grid.prune(new_grid),
                                                new_coord,
                                            )
                                        is
                                            Ok(solution) ->
                                                Solution(solution) |> Break

                                            Err(_) ->
                                                NoSolution(grid, current_coord)
                                                |> Continue

                                    Err(_) -> Solution(new_grid) |> Break
                            else
                                NoSolution(grid, current_coord) |> Continue

                        Solution(grid) ->
                            Solution(grid) |> Break,
            )

            when test_nums_result is
                NoSolution(_, _) ->
                    Err(NoSolutionFound)

                Solution(grid) ->
                    Ok(grid)

