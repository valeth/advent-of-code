open Common

let score ch =
  match ch with
  | '(' -> 1
  | '[' -> 2
  | '{' -> 3
  | '<' -> 4
  | _   -> 0

let rec score_remaining stack points =
  match stack with
  | [] -> points
  | x :: xs -> score_remaining xs (points * 5 + (score x))

let rec score_closing line stack =
  match (line, stack) with
  | ([], _) -> Some (score_remaining stack 0)
  | (x :: xs, y :: ys) when is_closing x ->
    if is_pair y x
    then score_closing xs ys
    else None
  | (x :: xs, _) ->
    score_closing xs (x :: stack)

let () =
  let path = Sys.argv.(1) in

  read_file path
  |> List.filter_map (fun cs -> score_closing cs [])
  |> List.sort compare
  |> fun lst -> List.nth lst (List.length lst / 2)
  |> print_int;

  print_endline ""
