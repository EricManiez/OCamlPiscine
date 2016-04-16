let () =
print_endline "----------------------------------------	";
	let dalekArmy = new Army.army in
	dalekArmy#delete;
	dalekArmy#add (new Dalek.dalek);
	(* dalekArmy#add (new Doctor.doctor); *)
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#delete;
	dalekArmy#delete;
	dalekArmy#delete;
	dalekArmy#add (new Dalek.dalek);
print_endline "----------------------------------------	";
	let doctorArmy = new Army.army in
	doctorArmy#delete;
	doctorArmy#add (new Doctor.doctor);
	(* peopleArmy#add (new People.people); *)
	doctorArmy#add (new Doctor.doctor);
	doctorArmy#add (new Doctor.doctor);
	doctorArmy#delete;
	doctorArmy#delete;
	doctorArmy#delete;
	doctorArmy#add (new Doctor.doctor);
print_endline "----------------------------------------	";
	let peopleArmy = new Army.army in
	peopleArmy#delete;
	peopleArmy#add (new People.people);
	(* dalekArmy#add (new Doctor.doctor); *)
	peopleArmy#add (new People.people);
	peopleArmy#add (new People.people);
	peopleArmy#delete;
	peopleArmy#delete;
	peopleArmy#delete;
	peopleArmy#add (new People.people);
print_endline "----------------------------------------	"