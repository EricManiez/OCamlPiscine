let rec ft_power n p =
	if p > 0
	then n * (ft_power n (p-1))
	else 1

let rec print_num h t u =
	if h < 10
	then (
		if t < 10
		then (
			if u < 10
			then (
				print_int h;
				print_int t;
				print_int u;
				if h == 7
				then print_string "\n"
				else print_string ", ";
				print_num h t (u + 1)
			)
			else print_num h (t + 1) (t + 2)
		)
		else print_num (h + 1) (h + 2) (h + 3)
	)
	else print_string ""
	
let ft_print_comb () =
	print_num 0 1 2

(* ---------------------- TESTS ---------------------- *)

let main () = 
	ft_print_comb ()

let () = main ()