open Common

let () =
  let path = Sys.argv.(1) in

  read_file path
  |> List.filter_map (fun cs ->
    match scores cs [] 0 with
    | (0, r) -> Some(r)
    | (_, _) -> None
  )
  |> List.sort compare
  |> fun lst -> List.nth lst (List.length lst / 2)
  |> print_int;

  print_endline ""
