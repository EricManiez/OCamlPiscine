let read_file filename = 
	let lines = ref [] in
	let ic = open_in filename in
	try
	  while true; do
	    lines := input_line ic :: !lines
	  done; 
	  Array.of_list !lines
	with End_of_file ->
	  close_in ic;
	  Array.of_list !lines

let main argv =
	let jokes = read_file argv.(1) in
	Random.self_init ();
	print_endline (Array.get jokes (Random.int 5))

let () =
	let argv = Sys.argv in
	main argv