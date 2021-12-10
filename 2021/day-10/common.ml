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
