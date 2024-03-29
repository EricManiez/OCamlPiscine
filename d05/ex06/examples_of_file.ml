let string_of_char c =
	String.make 1 c

let get_str_lst ic =
	let line = input_line ic in
	let lst = ref [] in
	let word = ref "" in
	for i = 0 to String.length line - 1 do
		match line.[i] with
		| ',' -> (
				lst := !word :: !lst;
				word := ""
			)
		| c -> word := !word ^ (string_of_char c)
	done;
	lst := !word :: !lst;
	!lst

let get_pair ic =
	let lst = get_str_lst ic in
	match lst with
	| [] -> (Array.make 0 0.,"")
	| head :: tail -> (Array.of_list (List.rev (List.map float_of_string tail)), head)
 
let examples_of_file filename = 
	let radars = ref [] in
	let ic = open_in filename in
	try
		while true; do
	    	radars := (get_pair ic) :: !radars
	  	done; 
	  	!radars
	with End_of_file ->
	  	close_in ic;
	  	!radars

(* -------------------- TESTS -------------------- *)

let print_coordinates arr =
	for i = 0 to Array.length arr - 1 do
		print_float arr.(i);
		print_string "; "
	done
	
let print_pair pair =
	print_string "Coordinates for pair : [|";
	print_coordinates (fst pair);
	print_string "|]\n";
	Printf.printf "Final string : %s\n\n" (snd pair)

let print_file lst =
	List.iter print_pair lst
	
let main argv =
	print_file (examples_of_file argv.(1))

let () =
	let argv = Sys.argv in
	main argv