let sum x y =
	x +. y

let print_operation x y =
	Printf.printf "%f +. %f = %f\n" x y (sum x y)

let () =
	print_operation 3.14 3.14;
	print_operation 5.1 1.5;
	print_operation 0. 0.;
	print_operation (-15.5) (-15.5);
	print_operation 0. (-15.5)
	