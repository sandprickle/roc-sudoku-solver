app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }
import pf.Stdout
import pf.Stderr
import pf.Arg exposing [Arg]
# import pf.Path
# import pf.File
import Grid exposing [Grid, Cell]
import Solve
import Puzzles

main! : List Arg => Result {} _
main! = |raw_args|
    args = List.map raw_args Arg.display
    when List.get args 1 is
        Err _ ->
            Stdout.line! "Invalid usage"

        Ok "benchmark" ->
            runBenchmark! {}

        Ok filename ->
            Stderr.line! "TODO"

runBenchmark! : {} => Result {} _
runBenchmark! = |{}|
    _ = Stdout.line! "Running benchmarks..."

    puzzles =
        List.repeat Puzzles.puzzle1 100
        |> List.map Grid.from_str
        |> List.map Grid.prune

    results = List.map puzzles Solve.backtrack_simple

    _ = Stdout.line! "Finished solving 100 puzzles"
    Ok {}
