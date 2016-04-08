let rec ft_power n p =
	if p > 0
	then n * (ft_power n (p-1))
	else 1

let rec print_num h t =
	if h < 100
	then (
		if t < 100
		then (
			if h < 10 then print_int 0;
			print_int h;
			print_char ' ';
			if t < 10 then print_int 0;
			print_int t;
			if h == 98
			then print_string "\n"
			else print_string ", ";
			print_num h (t + 1)
		)
	else print_num (h + 1) (h + 2)
	)
else print_string ""
	
let ft_print_comb2 () =
	print_num 0 1

(* ---------------------- TESTS ---------------------- *)

let main () = 
	ft_print_comb2 ()

let () = main ()