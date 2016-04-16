
(* -------------------- ex05 -------------------- *)

let eu_dist arr1 arr2 =
	let acc = ref 0. in
	let upper = min (Array.length arr1) (Array.length arr2) - 1 in
	for i = 0 to upper do
		let a = arr1.(i) in
		let b = arr2.(i) in
		acc := !acc +. ((a *. a) -. (2. *. a *. b) +. (b *. b))
	done;
	sqrt !acc
	
(* -------------------- ex06 -------------------- *)

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

(* -------------------- ex07 -------------------- *)

type radar = float array * string

let print_coordinates arr =
	for i = 0 to Array.length arr - 1 do
		print_float arr.(i);
		print_string "; "
	done
	
let print_sorting (lst : (float * radar) list) =
	List.iter (fun c -> Printf.printf "Distance = %f - Coord : " (fst c); print_coordinates (fst (snd c)); print_endline (snd (snd c))) lst

let comp_tuple x y =
	compare (fst x) (fst y)
	
let one_nn (lst : radar list) (rad : radar) =
	let tl = List.map (fun c -> (eu_dist (fst c) (fst rad), c)) lst in
    let sorting = List.sort comp_tuple tl in
    (* print_sorting sorting; *)		(* uncomment to test!!*)
	let sorted = List.map snd sorting in
	snd (List.hd sorted)
	
(* -------------------- TESTS -------------------- *)

let print_pair pair =
	print_string "Coordinates for pair : [|";
	print_coordinates (fst pair);
	print_string "|]\n";
	Printf.printf "Final string : %s\n\n" (snd pair)

let print_file lst =
	List.iter print_pair lst
	
let main argv =
	let rad = (Array.make 7 0., "g") in
	print_endline (one_nn (examples_of_file argv.(1)) rad)

let () =
	let argv = Sys.argv in
	main argv