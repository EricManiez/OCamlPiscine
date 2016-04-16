class people name =
	object
		initializer print_endline ("new instance of people object ; name parameter : " ^ name)
		
		val name:string = name
		val hp:int = 100

		method to_string = "Name : " ^ name ^ "  ||  HP = " ^ string_of_int hp
		method talk = print_endline ("I’m " ^ name ^ "! Do you know the Doctor?")
		method die = print_endline "Aaaarghh!"
	end