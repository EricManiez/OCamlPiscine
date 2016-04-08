let rec ft_power n p =
	if p > 0
	then n * (ft_power n (p-1))
	else 1

(* ---------------------- TESTS ---------------------- *)

let ft_print_int_endline n =
	print_int n;
	print_char '\n'

let print_result n p =
	print_int n;
	print_string "^";
	print_int p;
	print_string " = ";
	ft_print_int_endline (ft_power n p)

let main () =
	print_result 2 4;
	print_result 3 0;
	print_result 3 3;
	print_result 2 8;
	print_result 0 5
	
let () = main ()