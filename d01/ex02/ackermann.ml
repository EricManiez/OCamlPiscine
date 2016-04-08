let rec ackermann m n =
	if m < 0 || n < 0 then (-1)
	else if m = 0 then (n + 1)
	else (
		if n = 0 then (ackermann (m - 1) 1)
		else (ackermann (m - 1) (ackermann m (n - 1)))
	)

(* -------------------- TESTS -------------------- *)

let print_result m n = 
	print_string "ackermann (";
	print_int m;
	print_string ") (";
	print_int n;
	print_string ") = ";
	print_int (ackermann m n);
	print_char '\n'

let main () =
	print_result (-1) 0;
	print_result (0) (-1);
	print_result (-1) (-1);
	print_result 0 0;
	print_result 0 1;
	print_result 1 0;
	print_result 1 1;
	print_result 2 3;
	print_result 4 1

let () = main ()