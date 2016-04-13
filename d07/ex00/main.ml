let () =
	let person = new People.people "Constable Edmund" in
	print_endline person#to_string;
	person#talk;
	person#die