app "sudoku"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        pf.Stderr,
        pf.Task.{ Task },
        pf.Arg,
        pf.Path.{ Path },
        pf.File.{ File },
        Sudoku.Solve,
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
                { solvableMsg, legalMsg, puzzle } = parseAndSummarize contents
                {} <- Stdout.line solvableMsg |> Task.await
                {} <- Stdout.line legalMsg |> Task.await
                Stdout.line puzzle

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

parseAndSummarize : Str -> { solvableMsg : Str, legalMsg : Str, puzzle : Str }
parseAndSummarize = \contents ->
    puzzle = Sudoku.Solve.puzzleFromStr contents
    {
        solvableMsg: Str.joinWith
            [
                "This puzzle is ",
                if Sudoku.Solve.isSolvable puzzle then
                    ""
                else
                    "not ",
                "solvable!",
            ]
            "",
        legalMsg: Str.joinWith
            [
                "This puzzle ",
                if Sudoku.Solve.isLegal puzzle then
                    "follows"
                else
                    "does not follow",
                " the rules of Sudoku!",
            ]
            "",
        puzzle: Sudoku.Solve.prettyPrint puzzle,
    }

