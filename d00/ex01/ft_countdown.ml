let ft_print_int_endline n =
	print_int n;
	print_char '\n'

let rec ft_countdown n =
	if n > 0
	then (
		ft_print_int_endline n;
		ft_countdown (n-1)
	)
	else ft_print_int_endline 0

(* ---------------------- TESTS ---------------------- *)

let print_result n =
	print_string "Countdown from ";
	print_int n;
	print_string " :\n";
	ft_countdown n

let main () =
	print_result 5;
	print_result 9;
	print_result 0;
	print_result (-1)

let () = main ()