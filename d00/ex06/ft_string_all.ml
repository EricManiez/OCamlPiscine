let is_digit c = '0' <= c && c <= '9'
let is_a c = c == 'a' || c == 'A'

let rec iter func str n =
	if n < (String.length str)
	then (
		if func (String.get str n)
		then iter func str (n + 1)
		else false
	)
	else (
		true
	)

let ft_string_all func str =
	iter func str 0

(* ---------------------- TESTS ---------------------- *)

let print_result func str =
	print_string str;
	print_string " : ";
	print_string (string_of_bool (ft_string_all func str));
	print_char '\n'

let main () =

	print_endline "is_digit :";
	print_result is_digit "";
	print_result is_digit "coucou";
	print_result is_digit "radar";
	print_result is_digit "0123456789";
	print_result is_digit "0123456a789";
	print_char '\n';
	print_endline "is_a :";
	print_result is_a "";
	print_result is_a "coucou";
	print_result is_a "radar";
	print_result is_a "aaaaaaaaaaaaa";
	print_result is_a "aAaAaAaAaAaA"

let () = main ()