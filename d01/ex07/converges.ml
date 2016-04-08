let converges f x n =
	let rec iter f x n =
		if n < 0 then (-1)
		else if n = 0 then x
		else iter f (f x) (n - 1)
	in
	if iter f x n = iter f x (n + 1) then true
	else false

(* -------------------- TESTS -------------------- *)
let sq x =
	x * x

let print_result s f x n = 
	print_string "iter ";
	print_string s;
	print_string " (";
	print_int x;
	print_string ")";
	print_string " (";
	print_int n;
	print_string ")";
	print_string " = ";
	print_string (string_of_bool (converges f x n));
	print_char '\n'

let main () =
	print_result "(fun x -> x * x)" (fun x -> x * x) 2 4;
	print_result "(fun x -> x * x)" (fun x -> x * x) (-2) 4;
	print_result "(fun x -> x * x)" (fun x -> x * x) (-2) (-4);
	print_result "(fun x -> x * 2)" (fun x -> x * 2) 2 4;
	print_result "sq" sq (-1) (4);
	print_result "sq" sq (-2) (4);
	print_result "(( * ) 2)" (( * ) 2) 2 5;
	print_result "(fun x -> x / 2)" (fun x -> x / 2) 2 3;
	print_result "(fun x -> x / 2)" (fun x -> x / 2) 2 2;
	print_result "(fun x -> x / 2)" (fun x -> x / 2) 16000 2

let () = main ()