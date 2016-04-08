let rec iter str n =
	if n < (String.length str)
	then (
		if String.get str n == String.get str (String.length str - 1 - n)
		then iter str (n + 1)
		else false
	)
	else (
		true
	)

let ft_is_palindrome str =
	iter str 0

(* ---------------------- TESTS ---------------------- *)

let print_result str =
	print_string str;
	print_string " - ";
	print_string (string_of_bool (ft_is_palindrome str));
	print_char '\n'

let main () =
	print_result "";
	print_result "coucou";
	print_result "radar";
	print_result "!srevne'l a ajed tiate eniahc ettec ,tiaf nE";
	print_result "0123456789";
	print_result "";
	print_result "coucou";
	print_result "aaaaaaaaaaaaa";
	print_result "aAaAaAaAaAaA";
	print_result "aAaAaAaAaAaAa";
	print_result "rada";
	print_result "madam";
	print_result "V"

let () = main ()