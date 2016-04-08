let is_rotatable c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

let rotate c =
	if is_rotatable c
	then (
		if c == 'z'
		then 'a'
		else if c == 'Z'
		then 'A'
		else
		char_of_int ((int_of_char c) + 1)
	)
	else c

let rec ft_rot_n n str =
	if n > 0
	then (
		ft_rot_n (n - 1) (String.map rotate str)
	)
	else str

(* ---------------------- TESTS ---------------------- *)

let print_result n str =
	print_int n;
	print_char ' ';
	print_string str;
	print_string " - ";
	print_string (ft_rot_n n str);
	print_char '\n'

let main () =
	print_result 1 "abcdefghijklmnopqrstuvwxyz";
	print_result 13 "abcdefghijklmnopqrstuvwxyz";
	print_result 42 "0123456789";
	print_result 2 "OI2EAS67B9";
	print_result 0 "Damned !";
	print_result 42 "";
	print_result 1 "NBzlk qnbjr !"

let () = main ()