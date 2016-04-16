class doctor name age sidekick =
	object (self)
		initializer print_endline ("new instance of doctor object ; name parameter : " ^ name)
		
		val name:string = name
		val mutable age:int = age
		val sidekick: People.people = sidekick
		val mutable hp:int = 100

		method get_name = name
		method to_string = "Name : " ^ name ^ "  ||  age = " ^ string_of_int age ^ "  ||  HP = " ^ string_of_int hp ^ "  ||  sidekick : " ^ sidekick#to_string
		method talk = print_string ("Hi! I’m the Doctor!\n")
		method travel_in_time (start:int) (arrival:int) =
			age <- age;
			print_endline  
"_______(_@_)_______
| POLICE      BOX |
|_________________|
 | _____ | _____ |
 | |###| | |###| |
 | |###| | |###| | 
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ |$_____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ | _____ |
 | || || | || || | 
 | ||_|| | ||_|| | 
 |       |       | 
 *****************"
		method use_sonic_screwdriver = print_string "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii\n"
		method private regenerate = 
			print_endline (name ^ " has regenerated!");
			self#set_hp 100
		method set_hp n = 
			if n > 0 then hp <- n
			else hp <- 0;
			print_endline (name ^ " has " ^ string_of_int hp ^ " health points left.");
			if hp = 0 then self#regenerate					
		method take_damage n = 
			if hp > 0 then (
				print_endline (name ^ " takes " ^ string_of_int n ^ " damage !");
				self#set_hp (hp - n)
			)
			else print_endline (name ^ " is clearly dead already. Leave him alone!");
		method attack_dalek (dalek:Dalek.dalek) =
			Random.self_init ();
			let damage = Random.int 151 in
			print_endline (name ^ " attacks " ^ (dalek#get_name) ^ " for " ^ string_of_int damage ^ " damage !");
			dalek#take_damage damage
		method seppuku =
			Random.self_init ();
			let damage = Random.int 151 in
			print_endline (name ^ ", for some odd reason, decides to commit seppuku, and suffers " ^ string_of_int damage ^ " damage !");
			self#take_damage damage
	end