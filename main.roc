app "sudoku"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Arg,
        pf.Path.{ Path },
        pf.File.{ File },
        Grid.{ Grid, Cell },
        Solve,
        Puzzles,
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    if List.len args <= 1 then
        Stdout.line "Usage: sudoku <file>"
    else
        when List.get args 1 is
            Ok arg ->
                if arg == "benchmark" then
                    runBenchmark
                else
                    contents <- loadFile arg |> Task.await
                    inPuzzle = Grid.fromStr contents

                    _ <- inPuzzle
                        |> Grid.prettyPrint
                        |> Stdout.line
                        |> Task.await

                    output =
                        when inPuzzle |> Grid.prune |> Solve.backtrackSimple is
                            Ok solution ->
                                "Solution:\n$(Grid.prettyPrint solution)"

                            Err TooFewHints ->
                                "Too few hints!"

                            Err NotLegal ->
                                "Puzzle is not legal!"

                            Err NoSolutionFound ->
                                "No Solution!"
                    Stdout.line output

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

runBenchmark : Task {} I32
runBenchmark =
    puzzles =
        List.repeat Puzzles.puzzle1 100
        |> List.map Grid.fromStr
        |> List.map Grid.prune

    results = List.map puzzles Solve.backtrackSimple

    Task.ok {}
