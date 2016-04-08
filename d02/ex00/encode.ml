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

(* -------------------- TESTS -------------------- *)

let rec print_string_list lst =
	match lst with
	| [] -> print_string ""
	| head :: second :: tail -> print_char '"'; print_string head; print_char '"'; print_char ';'; print_string_list (second :: tail)
	| head :: tail -> print_char '"'; print_string head; print_char '"'; print_string_list tail
	
let rec print_int_list lst =
	match lst with
	| [] -> print_string ""
	| head :: second :: tail -> print_int head; print_char ';'; print_int_list (second :: tail)
	| head :: tail -> print_int head; print_int_list tail

let rec print_pair_list_int lst =
	match lst with
	| [] -> print_string ""
	| head :: second :: tail -> 
			print_char '(';
			print_int (fst head);
			print_char ' ';
			print_int (snd head);
			print_char ')';
			print_char ';';
			print_pair_list_int (second :: tail)
	| head :: tail -> 
			print_char '(';
			print_int (fst head);
			print_char ' ';
			print_int (snd head);
			print_char ')';
			print_pair_list_int tail

let rec print_pair_list_str lst =
	match lst with
	| [] -> print_string ""
	| head :: second :: tail -> 
			print_char '(';
			print_int (fst head);
			print_char ' ';
			print_char '"';
			print_string (snd head);
			print_char '"';
			print_char ')';
			print_char ';';
			print_pair_list_str (second :: tail)
	| head :: tail -> 
			print_char '(';
			print_int (fst head);
			print_char ' ';
			print_char '"';
			print_string (snd head);
			print_char '"';
			print_char ')';
			print_pair_list_str tail

let print_result_str d =
	print_string "encode ";
	print_char '[';
	print_string_list d; 
	print_char ']';
	print_string " => ";
	print_char '[';
	print_pair_list_str (encode d);
	print_char ']';
	print_char '\n'

let print_result_int d =
	print_char '[';
	print_int_list d; 
	print_char ']';
	print_string " => ";
	print_char '[';
	print_pair_list_int (encode d);
	print_char ']';
	print_char '\n'

let () =
	print_result_int [];
	print_result_int [1];
	print_result_int [1;1;1;1;1];
	print_result_int [1;1;2;2;3;3;3;4;4;4;4;5;5;5;5;5];
	print_result_str ["us"];
	print_result_str ["us";"us";"us";"us"];
	print_result_str ["us";"us";"us";"uas";"uas";"us";"uas"]
