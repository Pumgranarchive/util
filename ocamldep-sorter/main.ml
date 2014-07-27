let usage bin_name = "Usage: " ^ bin_name ^" [-cmx -cmo] dep_file"

let parse file_name =
  lwt file = Lwt_io.open_file ~mode:Lwt_io.input file_name in

  let clean str =
    let regexp = Str.regexp " *" in
    Str.global_replace regexp "" str
  in
  let get_file_name line =
    let regexp = Str.regexp ":" in
    let ret = Str.split regexp line in
    clean (List.nth ret 0)
  in
  let get_dep line =
    let regexp = Str.regexp " +" in
    let cleaned_line = Str.global_replace regexp " " line in
    let regexp = Str.regexp ":" in
    let ret = Str.split_delim regexp cleaned_line in
    let tmp = if List.length ret < 2 then "" else List.nth ret 1 in
    let space_regexp = Str.regexp " " in
    let ret = Str.split space_regexp tmp in
    List.map clean ret
  in
  let not_finished line =
    let regexp = Str.regexp ".*\\" in
    Str.string_match regexp line 0
  in
  let clean_not_finished_line line =
    let regexp = Str.regexp "\\" in
    Str.global_replace regexp "" line
  in
  let rec read dep_list rest =
    try_lwt

      lwt line = Lwt_io.read_line file in
      let line = rest ^ line in

      if not_finished line
      then read dep_list (clean_not_finished_line line)
      else read ((get_file_name line, get_dep line)::dep_list) ""

    with End_of_file ->
      Lwt.return dep_list
  in
  lwt hard_list = read [] "" in
  lwt () = Lwt_io.close file in
  Lwt.return hard_list

let sort hard_list =
  let exist x y = String.compare x y == 0 in
  let not_exist x y = not (exist x y) in
  let rec add name dep blist sorted_list =
    if List.length dep = 0
    then (List.rev (name::blist))@sorted_list
    else match sorted_list with
    | []        -> List.rev blist
    | n::tail   ->
      let new_dep = List.filter (not_exist n) dep in
      add name new_dep (n::blist) tail
  in
  let rec sort_aux rest_list sorted_list = function
    | []                -> rest_list, sorted_list
    | (name, dep)::tail ->
      if List.for_all (not_exist name) sorted_list then
        (if List.for_all (fun d -> List.exists (exist d) sorted_list) dep
         then sort_aux rest_list (add name dep [] sorted_list) tail
         else sort_aux ((name, dep)::rest_list) sorted_list tail)
      else sort_aux rest_list sorted_list tail
  in
  let rec manager previous_rest_length sorted_list hard_list =
    let rest_list, sorted_list = sort_aux [] sorted_list hard_list in
    let current_rest_length = List.length rest_list in
    if current_rest_length >= previous_rest_length
    then failwith "Cercular dependency";
    if current_rest_length > 0
    then manager current_rest_length sorted_list rest_list
    else sorted_list
  in
  manager (List.length hard_list) [] hard_list

let get_args () =
  let bin_name = Array.get Sys.argv 0 in
  if Array.length Sys.argv < 2
  then failwith (usage bin_name)
  else
    let rec aux pos cmx cmo file_name =
      if Array.length Sys.argv > pos then
        let arg = Array.get Sys.argv pos in
        let score_regexp = Str.regexp "^-" in
        if Str.string_partial_match score_regexp arg 0
        then (if String.compare arg "-cmx" = 0
          then aux (pos+1) true cmo file_name
          else if String.compare arg "-cmo" = 0
          then aux (pos+1) cmx true file_name
          else failwith ("Invalid arg: " ^ arg))
        else aux (pos+1) cmx cmo (Some arg)
      else cmx, cmo, file_name
    in
    let cmx, cmo, file_name = aux 1 false false None in
    match file_name with
    | None   -> failwith (usage bin_name)
    | Some f -> cmx, cmo, f

let type_filter type_name name =
  let regexp = Str.regexp "\\." in
  let type_n = List.nth (Str.split regexp name) 1 in
  String.compare type_n type_name = 0 || String.compare type_n "cmi" = 0

let main () =
  let cmx, cmo, file_name = get_args () in
  lwt hard_list = parse file_name in
  let sorted_list = sort hard_list in
  let f_filter = match cmx, cmo with
    | true, false -> type_filter "cmx"
    | false, true -> type_filter "cmo"
    | _, _        -> (fun x -> true)
  in
  let filtered_list = List.filter f_filter sorted_list in
  List.iter (fun s -> Printf.printf "%s " s) filtered_list;
  Lwt.return ()

lwt _ =
   try_lwt main ()
   with e -> Lwt_io.printl (Printexc.to_string e)
