let rec list_make n x =
	match n with
	| 0 -> []
	| _ -> [x] @ list_make (n -1) x

let get_list n =
	(list_make n (new Atoms.carbon)) @ (list_make (n * 2 + 2) (new Atoms.hydrogen))

class alkane n =
	object
		inherit Abstract.molecule 
		(
			match n with
			| 1 -> "Methane"
			| 2 -> "Ethane"
			| 3 -> "Propane"
			| 4 -> "Butane"
			| 5 -> "Pentane"
			| 6 -> "Hexane"
			| 7 -> "Heptane"
			| 8 -> "Octane"
			| 9 -> "Nonane"
			| 10 -> "Decane"
			| 11 -> "Undecane"
			| 12 -> "Dodecane"
			| 13 -> "Tridecane"
			| 14 -> "Tetradecane"
			| 15 -> "Pentadecane"
			| 16 -> "Hexadecane"
			| 17 -> "Heptadecane"
			| 18 -> "Octadecane"
			| 19 -> "Nonadecane"
			| 20 -> "Icosane"
			| 30 -> "Triacontane"
			| 40 -> "Tetracontane"
			| 50 -> "Pentacontane"
			| 60 -> "Hexacontane"
			| _ -> "Something-ane"
		)
		(get_list n)
	end

class methane =
	object
		inherit alkane 1
	end

class ethane =
	object
		inherit alkane 2
	end

class pentane =
	object
		inherit alkane 5
	end

class octane =
	object
		inherit alkane 8
	end

class hexadecane =
	object
		inherit alkane 16
	end

class icosane =
	object
		inherit alkane 20
	end

class triacontane =
	object
		inherit alkane 30
	end

class tetracontane =
	object
		inherit alkane 40
	end

class pentacontane =
	object
		inherit alkane 50
	end

class hexacontane =
	object
		inherit alkane 60
	end

class alkane_combustion (a_l : alkane list) =
object (self)
	inherit Abstract.reaction a_l
	method get_start = 
	if not self#is_balanced then failwith "reaction is not balanced! :-("
	else (
	)

	(* : (molecule * int) list *)
	method get_result : (molecule * int) list
	method balance : reaction
	method is_balanced = true
end
