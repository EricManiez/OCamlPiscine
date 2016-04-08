let rec is_in_list el lst =
	match lst with
	| [] -> false
	| head :: tail -> el = head || is_in_list el tail

let rec crossover lst1 lst2 =
	match lst1 with
	| [] -> []
	| head :: tail -> 
		if is_in_list head lst2 then [head] @ crossover tail lst2
		else crossover tail lst2

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

let print_result_str lst1 lst2 =
	print_string "List 1 : [";
	print_string_list lst1; 
	print_string "]\nList 2 : [";
	print_string_list lst2; 
	print_string "]\nCommon elements : ";
	print_char '[';
	print_string_list (crossover lst1 lst2);
	print_string "]\n\n"

let print_result_int lst1 lst2 =
	print_string "List 1 : [";
	print_int_list lst1; 
	print_string "]\nList 2 : [";
	print_int_list lst2; 
	print_string "]\nCommon elements : ";
	print_char '[';
	print_int_list (crossover lst1 lst2);
	print_string "]\n\n"

let () =
	print_result_int [] [1;1;2;2;3;3;3;4;4;4;4;5;5;5;5;5];
	print_result_int [1;1;2;2;3;3;3;4;4;4;4;5;5;5;5;5] [];
	print_result_int [] [];
	print_result_int [1;1;1;1;1;2;5] [1;1;2;2;3;3;3;4;4;4;4;5;5;5;5;5];
	print_result_str ["us";"us";"us";"us"] ["us";"us";"us";"uas";"uas";"us";"uas"]
