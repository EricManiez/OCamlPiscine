let rec getcase (x:int) (y:int) map = match map with
	| [] -> invalid_arg "Error y (getcase)"
	| h1::tail -> (
		if y > 0 then getcase x (y - 1) tail
		else (
			let rec loop1 x elem =
				match elem with
				| [] -> invalid_arg "Error x (getcase)"
				| e1::tail -> if x > 0 then loop1 (x - 1) tail else e1
			in
			loop1 x h1
		)
	)

let get_Case_status (a, b, c, d) = d

let do_check x y z =
	if x = y && x = z && x <> "0" then true
	else false

let check map =
	let c1 = (get_Case_status (List.nth (List.nth map 0) 0)) in
	let c2 = (get_Case_status (List.nth (List.nth map 0) 1)) in
	let c3 = (get_Case_status (List.nth (List.nth map 0) 2)) in
	let c4 = (get_Case_status (List.nth (List.nth map 1) 0)) in
	let c5 = (get_Case_status (List.nth (List.nth map 1) 1)) in
	let c6 = (get_Case_status (List.nth (List.nth map 1) 2)) in
	let c7 = (get_Case_status (List.nth (List.nth map 2) 0)) in
	let c8 = (get_Case_status (List.nth (List.nth map 2) 1)) in
	let c9 = (get_Case_status (List.nth (List.nth map 2) 2)) in
	if (do_check c1 c2 c3) = true then c1
	else if (do_check c4 c5 c6) = true then c4
	else if (do_check c7 c8 c9) = true then c7
	else if (do_check c1 c4 c7) = true then c1
	else if (do_check c2 c5 c8) = true then c2
	else if (do_check c3 c6 c9) = true then c3
	else if (do_check c1 c5 c9) = true then c1
	else if (do_check c3 c5 c7) = true then c3
	else "0"

let rec print_map1 map = match map with
	| [] -> print_string ""
	| h1::tail -> (
		let rec loop elem line =
			match elem with
			| [] -> print_string ""
			| e1::[]-> Case.line_print e1 line
			| e1::tail -> (
			 	Case.line_print e1 line;
			 	print_string " |  ";
			 	loop tail line
			)
		in
		loop h1 0;
		print_char '\n';
		loop h1 1;
		print_char '\n';
		loop h1 2;
		if tail <> [] then (
			print_char '\n';
			print_endline "-------------------------";
			print_map1 tail
		)
	)