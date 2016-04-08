let string_of_char c =
	String.make 1 c

let prepend_char c str =
	(string_of_char c) ^ str

let rec lst_conc l1 l2 =
  	match l1 with
	| [] -> l2
	| head :: tail -> head :: (lst_conc tail l2)

let rec rev_lst lst =
	match lst with
	| [] -> []
	| head :: tail -> lst_conc (rev_lst tail) [head]

let rec map f lst =
	match lst with
	| [] -> []
	| head :: tail -> lst_conc [f head] (map f tail)

let rec flip_n_conc n =
	match n with
	| 1 -> ["0"; "1"]
	| _ when n > 0 -> lst_conc (map (prepend_char '0') (flip_n_conc (n - 1))) (map (prepend_char '1') (rev_lst (flip_n_conc (n - 1))))
	| _ -> []

let gray n =
	print_endline (String.concat " " (flip_n_conc n))

(* -------------------- TESTS -------------------- *)

let print_result n =
	Printf.printf ("Gray code - %d bits :\n") n;
	gray n

let () =
	print_result (-1);
	print_result 0;
	print_result 1;
	print_result 2;
	print_result 3;
	print_result 4;
	print_result 5
