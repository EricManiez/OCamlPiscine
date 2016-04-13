class galifrey (daA:Dalek.dalek list) (doA:Doctor.doctor list) (peA:People.people list) =
	object (self)
		val dalekArmy = daA
		val doctorArmy = doA
		val peopleArmy = peA

		method isAliveDalekArmy (dalekArmy:Dalek.dalek list) =
			match dalekArmy with
			| [] -> false
			| head :: tail -> if head#get_hp > 0 then true else self#isAliveDalekArmy tail
		
		method isAliveDoctorArmy (doctorArmy:Doctor.doctor list) =
			match doctorArmy with
			| [] -> false
			| head :: tail -> if head#get_hp > 0 then true else self#isAliveDoctorArmy tail
		
		method isAlivePeopleArmy (peopleArmy:People.people list) =
			match peopleArmy with
			| [] -> false
			| head :: tail -> if head#get_hp > 0 then true else self#isAlivePeopleArmy tail

		method attackRandomPeople (dalek:Dalek.dalek) =
			Random.self_init ();
			let randomPerson = List.nth peopleArmy (Random.int (List.length peopleArmy)) in
			dalek#attack_people randomPerson

		method attackRandomDalek (doc:Doctor.doctor) =
			Random.self_init ();
			let randomDalek = List.nth dalekArmy (Random.int (List.length dalekArmy)) in
			doc#attack_dalek randomDalek

		method dalekOffensive =
			print_endline "\n--- DALEK OFFENSIVE ---\n";
			if self#isAliveDalekArmy dalekArmy then (
				if self#isAlivePeopleArmy peopleArmy
				then List.iter self#attackRandomPeople dalekArmy
				else print_endline "The Daleks have already torn through the ranks of the hopeless People..."
			)
			else print_endline "Not a single living Dalek remains..."

		method doctorOffensive =
			print_endline "\n--- DOCTOR OFFENSIVE ---\n";
			if self#isAliveDoctorArmy doctorArmy then (
				if self#isAliveDalekArmy dalekArmy
				then List.iter self#attackRandomDalek doctorArmy
				else print_endline "The doctors have already torn through the ranks of the hopeless Daleks..."
			)
			else print_endline "Not a single living doctor remains..."

		method do_time_war =
			self#doctorOffensive;
			self#dalekOffensive;
			self#doctorOffensive;
			self#doctorOffensive;
			self#doctorOffensive;
			self#doctorOffensive;
			self#dalekOffensive
	end
