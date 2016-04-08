let ft_test_sign n =
	if n >= 0
	then print_endline "positive"
	else print_endline "negative"

(* ---------------------- TESTS ---------------------- *)

let print_result n = 
	print_int n;
	print_string " => ";
	ft_test_sign n

let main () =
	print_result 42;
	print_result 0;
	print_result (-42)

let () = main ()