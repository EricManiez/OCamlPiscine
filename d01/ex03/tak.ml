let rec tak x y z =
	if y < x then (
		tak (tak (x - 1) y z) (tak (y -1) z x) (tak (z -1) x y)
	)
	else z

(* -------------------- TESTS -------------------- *)

let print_result x y z = 
	print_string "tak (";
	print_int x;
	print_string ") (";
	print_int y;
	print_string ") (";
	print_int z;
	print_string ") = ";
	print_int (tak x y z);
	print_char '\n'

let main () =
	print_result 0 (-1) 0;
	print_result 0 (0) (-1);
	print_result 0 (-1) (-1);
	print_result (-6) (0) (12);
	print_result 1 2 3;
	print_result 5 23 7;
	print_result 9 1 0;
	print_result 1 1 1;
	print_result 0 42 0;
	print_result 23498 98734 98776

let () = main ()