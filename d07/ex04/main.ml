let () =
	let edmund = new People.people "Constable Edmund" in
print_endline "----------------------------------------	";
	let dalekArmy = new Army.army in
	dalekArmy#delete;
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
	dalekArmy#add (new Dalek.dalek);
print_endline "----------------------------------------	";
	let doctorArmy = new Army.army in
	doctorArmy#delete;
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
	doctorArmy#add (new Doctor.doctor "Inspector Spacetime" 42 edmund);
print_endline "----------------------------------------	";
	let peopleArmy = new Army.army in
	peopleArmy#delete;
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
	peopleArmy#add (new People.people "Constable Edmund");
print_endline "----------------------------------------	";
	let gal = new Galifrey.galifrey (dalekArmy#get_soldiers) (doctorArmy#get_soldiers) (peopleArmy#get_soldiers) in
	gal#do_time_war;
print_endline "----------------------------------------	"