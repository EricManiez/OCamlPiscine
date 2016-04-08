let ft_sum f l u =
	let rec sum_aux i acc =
		if u < l then nan
		else if i < l then acc
		else sum_aux (i - 1) (acc +. (f i))
	in
	sum_aux u 0.
	
(* -------------------- TESTS -------------------- *)

let square_float x =
	float_of_int (x * x)

let half_float x =
	float_of_int (x / 2)

let print_result s f l u = 
	print_string "ft_sum ";
	print_string s;
	print_string " (";
	print_int l;
	print_string ")";
	print_string " (";
	print_int u;
	print_string ")";
	print_string " = ";
	print_float (ft_sum f l u);
	print_char '\n'

let main () =
	print_result "square_float" square_float 1 10;
	print_result "square_float" square_float 2 2;
	print_result "square_float" square_float 2 4;
	print_result "square_float" square_float (-2) 4;
	print_result "square_float" square_float (-2) (-4);
	print_result "square_float" square_float 2 4;
	print_result "half_float" half_float 2 3;
	print_result "half_float" half_float 2 2;
	print_result "half_float" half_float 16000 2

let () = main ()