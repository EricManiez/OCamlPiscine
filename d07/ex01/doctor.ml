class doctor name age sidekick =
	object
		initializer print_endline ("new instance of doctor object ; name parameter : " ^ name)
		
		val name:string = name
		val mutable age:int = age
		val sidekick: People.people = sidekick
		val mutable hp:int = 100

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
		method private regenerate = hp <- 100
	end