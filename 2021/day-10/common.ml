let rec read_line in_chan =
  try
    let line = input_line in_chan in
    line :: read_line in_chan
  with End_of_file ->
    close_in in_chan;
    []

let char_list str = List.init (String.length str) (String.get str)

let read_file path = open_in path |> read_line |> List.map char_list

let is_closing ch = List.mem ch [')'; ']'; '}'; '>']

let is_pair o c =
  match (o, c) with
  | ('(', ')') -> true
  | ('[', ']') -> true
  | ('{', '}') -> true
  | ('<', '>') -> true
  | (_, _)     -> false

let rec rem_score stack points =
  let score ch =
    match ch with
    | '(' -> 1
    | '[' -> 2
    | '{' -> 3
    | '<' -> 4
    | _   -> 0
  in

  match stack with
  | [] -> points
  | x :: xs -> rem_score xs (points * 5 + (score x))

let err_score ch =
  match ch with
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _   -> 0


let rec scores line stack error_score =
  match (line, stack) with
  | ([], _) -> (error_score, rem_score stack 0)
  | (x :: xs, y :: ys) when is_closing x ->
    if is_pair y x
    then scores xs ys error_score
    else scores xs ys (error_score + err_score x)
  | (x :: xs, _) ->
    scores xs (x :: stack) error_score
