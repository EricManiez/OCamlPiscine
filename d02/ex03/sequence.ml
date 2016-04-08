let string_of_char c =
	String.make 1 c

let explode s =
  	let rec expl i l =
    	if i < 0 then l
    	else expl (i - 1) ((string_of_char s.[i]) :: l)
    in
  	expl (String.length s - 1) []

let rec map f lst =
	match lst with
	| [] -> []
	| head :: tail -> [f head] @ (map f tail)

let conc_str_pair p =
	string_of_int (fst p) ^ snd p

let rec conc_str_pair_lst lst =
	match lst with
	| [] -> ""
	| head :: tail -> (conc_str_pair head) ^ conc_str_pair_lst tail

(* -------------------- BEGIN ENCODE BLOCK -------------------- *)

let rec iter i lst =
	match lst with
	| [] -> 0
	| head :: second :: tail ->
			if head = second then iter (i + 1) (second :: tail)
			else i
	| head :: tail -> i

let rec get_next_symbol lst =
	match lst with
	| [] -> []
	| head :: second :: tail ->
			if head = second then get_next_symbol (second :: tail)
			else (second :: tail)
	| head :: tail -> tail

let rec encode lst =
	match lst with
	| [] -> []
	| head :: tail -> [(iter 1 lst, head)] @ (encode (get_next_symbol lst))

(* -------------------- END ENCODE BLOCK -------------------- *)

let rec sequence n =
	match n with
	| 1 -> "1"
	| _ when n > 0 -> conc_str_pair_lst (encode(explode (sequence (n - 1))))
	| _ -> ""

(* -------------------- TESTS -------------------- *)

let print_result n =
	Printf.printf ("Iteration %d of sequence :\n%s\n") n (sequence n)


let () =
	print_result (-1);
	print_result 0;
	print_result 1;
	print_result 2;
	print_result 3;
	print_result 4;
	print_result 5;
	print_result 6;
	print_result 7;
	print_result 8;
	print_result 9;
	print_result 10;
	print_result 15;
	print_result 18