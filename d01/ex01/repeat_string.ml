let rec repeat_string ?s:(str="x") n =
	if n < 0
	then "Error"
	else if n > 0
	then str ^ (repeat_string ~s:str (n - 1))
	else ""

(* -------------------- TESTS -------------------- *)

let print_result ?s:(str="x") n = 
	print_string "repeat_x (";
	print_int n;
	print_string ") : ";
	print_endline (repeat_string ~s:str n)

let main () =
	print_result (-1);
	print_result (0);
	print_result 1;
	print_result 2;
	print_result 3;
	print_result 42;
	print_result ~s:"" (-1);
	print_result ~s:"" (0);
	print_result ~s:"" 1;
	print_result ~s:"" 2;
	print_result ~s:"" 3;
	print_result ~s:"" 42;
	print_result ~s:"BigKahunaBurger" (-1);
	print_result ~s:"BigKahunaBurger" (0);
	print_result ~s:"BigKahunaBurger" 1;
	print_result ~s:"BigKahunaBurger" 2;
	print_result ~s:"BigKahunaBurger" 3;
	print_result ~s:"BigKahunaBurger" 42


let () = main ()