class ['a] army  =
	object
		initializer print_endline "An army is born."

		val mutable soldiers = []

		method add (n:'a) = 
			soldiers <- [n] @ soldiers;
			print_endline ("ADD - army size after operation : " ^ string_of_int (List.length soldiers))
		method delete = 
			soldiers <- (match soldiers with | [] -> [] | head :: tail -> tail);
			print_endline ("DELETE - army size after operation : " ^ string_of_int (List.length soldiers))
	end