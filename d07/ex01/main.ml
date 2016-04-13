let () =
	let edmund = new People.people "Constable Edmund" in
	let inspector = new Doctor.doctor "Inspector Spacetime" 42 edmund in
	print_endline inspector#to_string;
	inspector#talk;
	inspector#travel_in_time 2000 0;
	inspector#use_sonic_screwdriver
