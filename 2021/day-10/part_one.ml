open Common

let score ch =
  match ch with
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _   -> 0

let rec count_errors line stack errors =
  match (line, stack) with
  | ([], _) -> errors
  | (x :: xs, y :: ys) when is_closing x ->
    if is_pair y x
    then count_errors xs ys errors
    else count_errors xs ys (errors + score x)
  | (x :: xs, _) ->
    count_errors xs (x :: stack) errors

let () =
  let path = Sys.argv.(1) in

  read_file path
  |> List.map (fun cs -> count_errors cs [] 0)
  |> List.fold_left (+) 0
  |> print_int;

  print_endline ""
