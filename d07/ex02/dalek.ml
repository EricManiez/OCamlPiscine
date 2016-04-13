let string_of_char_of_int n = 
	String.make 1 (char_of_int n)

let random_name =
	Random.self_init ();
	string_of_char_of_int (Random.int 94 + 33) ^ string_of_char_of_int (Random.int 94 + 33) ^ string_of_char_of_int (Random.int 94 + 33)

let rec list_accessor lst n =
	match lst with
	| [] -> ""
	| head :: tail ->
		match n with
		| 0 -> head
		| _ -> list_accessor tail (n - 1)

class dalek =
	object (self)
		val name:string = "Dalek" ^ random_name
		val mutable hp:int = 100
		val mutable shield:bool = true
		
		method get_name = name
		method to_string =  "Name : " ^ name ^ "  ||  HP = " ^ string_of_int hp ^ "  ||  shield : " ^ string_of_bool shield
		method talk =
			let phrases = ["Explain! Explain!" ; "Exterminate! Exterminate!" ; "I obey!" ; "You are the Doctor! You are the enemy of the Daleks!"] in
			Random.self_init ();
			print_endline (list_accessor phrases (Random.int 4))
		method exterminate (person:People.people) =
			shield <- not shield;
			person#die
		method die = print_endline "Emergency Temporal Shift!"
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
		method attack_people (person:People.people) =
			Random.self_init ();
			let damage = Random.int 151 in
			print_endline (name ^ " attacks " ^ (person#get_name) ^ " for " ^ string_of_int damage ^ " damage !");
			person#take_damage damage

	end