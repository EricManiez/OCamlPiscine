let () =
	let edmund = new People.people "Constable Edmund" in
	let inspector = new Doctor.doctor "Inspector Spacetime" 42 edmund in
	let dalek = new Dalek.dalek in
	print_endline dalek#to_string;
	dalek#talk;
	dalek#talk;
	dalek#talk;
	dalek#talk;

print_endline "----------------------------------------	";

	dalek#attack_people edmund;
	dalek#attack_people edmund;	
	dalek#attack_people edmund;	
print_endline "----------------------------------------	";
	inspector#attack_dalek dalek;
	inspector#attack_dalek dalek;
	inspector#attack_dalek dalek;
print_endline "----------------------------------------	";
	inspector#seppuku;
	inspector#seppuku;
	inspector#seppuku
