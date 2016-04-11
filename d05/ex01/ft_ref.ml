type 'a ft_ref = {mutable contents: 'a}

let return n =
	{contents = n}

let get r =
	r.contents

let set r v =
	r.contents <- v

let bind r f =
	{contents = f r.contents}

let () =
	let r1 = return 1 in
	print_int (get r1);
	print_char '\n';
	set r1 42;
	print_int (get r1);
	print_char '\n';
	print_endline (get (bind r1 string_of_int));