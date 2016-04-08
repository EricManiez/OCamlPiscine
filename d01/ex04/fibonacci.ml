let fibonacci n =
	let rec fib_aux i a b =
		if n < 0 then (-1)
		else if i = n then a
		else fib_aux (i + 1) b (b + a)
	in fib_aux 0 0 1

(* -------------------- TESTS -------------------- *)

let print_result x = 
	print_string "fibonacci (";
	print_int x;
	print_string ") = ";
	print_int (fibonacci x);
	print_char '\n'

let main () =
	print_result (-6);
	print_result 0;
	print_result 1;
	print_result 3;
	print_result 5;
	print_result 6;
	print_result 9;
	print_result 13

let () = main ()