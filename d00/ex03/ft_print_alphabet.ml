let rec ft_loop letter = 
	if int_of_char letter <= int_of_char 'z'
	then (
		print_char letter;
		ft_loop (char_of_int((int_of_char letter) + 1))
	)
	else print_char '\n'

let ft_print_alphabet () = 
	ft_loop 'a'

(* ---------------------- TESTS ---------------------- *)

let main () =
	ft_print_alphabet()

let () = main ()