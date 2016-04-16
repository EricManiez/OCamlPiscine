class people name =
	object (self)
		initializer print_endline ("new instance of people object ; name parameter : " ^ name)
		
		val name:string = name
		val mutable hp:int = 100

		method get_name = name
		method get_hp = hp
		method to_string = "Name : " ^ name ^ "  ||  HP = " ^ string_of_int hp
		method talk = print_endline ("I’m " ^ name ^ "! Do you know the Doctor?")
		method die = print_endline "Aaaarghh!"; hp <- 0
		method set_hp n = 
			if n > 0 then hp <- n
			else hp <- 0;
			print_endline (name ^ " has " ^ string_of_int hp ^ " health points left.");
			if hp = 0 then self#die					
		method take_damage n = 
			if hp > 0 then (
				print_endline (name ^ " takes " ^ string_of_int n ^ " damage !");
				self#set_hp (hp - n)
			)
			else print_endline (name ^ " is clearly dead already. Leave him alone!");
	end