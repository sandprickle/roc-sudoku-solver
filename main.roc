app "sudoku"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Arg,
        pf.Path.{ Path },
        pf.File.{ File },
        Grid.{ Grid, Cell },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    if List.len args <= 1 then
        Stdout.line "Usage: sudoku <file>"
    else
        when List.get args 1 is
            Ok file ->
                contents <- loadFile file |> Task.await
                Grid.fromStr contents
                |> Grid.prettyPrint
                |> Stdout.line

            Err _ ->
                Task.err 1

loadFile : Str -> Task Str I32
loadFile = \pathStr ->
    path = Path.fromStr pathStr

    result <- File.readUtf8 path |> Task.attempt

    when result is
        Ok content ->
            content
            |> Task.ok

        Err _ ->
            {} <- Stderr.line "Error reading file" |> Task.await
            Task.err 1

# attemptSolve : Grid -> Result Grid _
# attemptSolve = \inputGrid ->
#     if Grid.legal inputGrid == Illegal then
#         Err IllegalPuzzle
#     else if Grid.sufficientHints inputGrid == TooFewHints then
#         Err TooFewHints
#     else
#         solve inputGrid

# solve : Grid -> Result Grid [NoSolution]
# solve = \grid ->
#     pruned = Grid.prune grid
#     possibilities = Grid.possibilities pruned
#     result = List.walkUntil

# crash "wip"

