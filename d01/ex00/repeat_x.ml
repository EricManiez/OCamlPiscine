let rec repeat_x n =
	if n < 0
	then "Error"
	else if n > 0
	then "x" ^ (repeat_x (n - 1))
	else ""

(* -------------------- TESTS -------------------- *)

let print_result n = 
	print_string "repeat_x (";
	print_int n;
	print_string ") : ";
	print_endline (repeat_x n)

let main () =
	print_result (-1);
	print_result (0);
	print_result 1;
	print_result 2;
	print_result 3;
	print_result 42


let () = main ()