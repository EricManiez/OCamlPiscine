let rec iter str n =
	if n < String.length str
	then (
		iter str (n + 1);
		print_char (String.get str n)
	)
	
let ft_print_rev str =
	iter str 0;
	print_char '\n'

(* ---------------------- TESTS ---------------------- *)

let main () =
	ft_print_rev "";
	ft_print_rev "coucou";
	ft_print_rev ")lol( radar";
	ft_print_rev "!srevne'l a ajed tiate eniahc ettec ,tiaf nE"

let () = main ()