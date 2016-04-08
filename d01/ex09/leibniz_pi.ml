let leibniz_pi d =
	let abs x =
		if x < 0. then x *. (-1.)
		else x
	in
	let rec ft_power n p =
		if p > 0
		then n * (ft_power n (p-1))
		else 1
	in
	let f x =
		float_of_int (ft_power (-1) x) /. float_of_int(2 * x + 1)
	in
	let rec sum_aux i acc =
		if d < 0. then (-1)
		else if (abs ((4. *. acc) -. (4. *. (atan 1.)))) < d then i + 1
		else sum_aux (i + 1) (acc +. (f i))
	in
	sum_aux 0 0.
	
(* -------------------- TESTS -------------------- *)

let square_float x =
	float_of_int (x * x)

let half_float x =
	float_of_int (x / 2)

let rec buffer x =
	if x > 0 then (
		print_char ' ';
		buffer (x - 1)
	)

let print_result d = 
	print_string "leibniz_pi | delta = ";
	print_string "(";
	print_float d;
	print_string ")";
	buffer (8 - String.length (string_of_float d));
	print_string " | ";
	print_string "iterations = ";
	print_int (leibniz_pi d);
	print_char '\n'

let main () =
	print_result (-4.);
	print_result 4.;
	print_result 3.;
	print_result 2.;
	print_result 1.;
	print_result 0.5;
	print_result 0.123;
	print_result 0.0123;
	print_result 0.00123;
	print_result 0.000123
	
let () = main ()