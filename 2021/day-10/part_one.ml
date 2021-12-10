open Common

let () =
  let path = Sys.argv.(1) in

  read_file path
  |> List.map (fun cs ->
    match scores cs [] 0 with
    | (e, _) -> e
  )
  |> List.fold_left (+) 0
  |> print_int;

  print_endline ""
