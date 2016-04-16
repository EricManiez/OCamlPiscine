class virtual atom (n:string) (sym:string) (a_n:int) =
object (self)
	val name = n
	val symbol = sym
	val atomic_number = a_n

	method name = name
	method symbol = symbol
	method atomic_number = atomic_number

	method to_string = name ^ " - " ^ symbol ^ " - " ^ string_of_int atomic_number
	method equals (atom:atom) = atomic_number = atom#atomic_number
	method print = print_endline self#to_string
end

let comp_atoms b a =
	let x = a#symbol in
	let y = b#symbol in
	match x with
	| "C" -> (
		match y with
			| "C" -> 0
			| _ -> 5
		)
	| "H" -> (
		match y with
			| "C" -> (-5)
			| "H" -> 0
			| _ -> 5
		)
	| _ -> (
		match y with
		| "C" -> (-5)
		| "H" -> (-5)
		| _ -> String.compare y x
	)
		
class virtual molecule n (a_l:atom list) =
object (self)
	val name = n
	val atoms_list = List.sort comp_atoms a_l

	method private count_atom (symbol:string) = List.length (List.filter (fun c -> c#symbol = symbol) atoms_list)

	method private iter_atoms uniq_atoms_list = 
		match uniq_atoms_list with
		| [] -> ""
		| head :: tail ->
			begin
				match self#count_atom head#symbol with
				| 0 -> ""
				| 1 -> head#symbol
				| _ -> head#symbol ^ string_of_int (self#count_atom head#symbol)
			end ^ self#iter_atoms tail

	method name = name
	method formula = 
		(* List.iter (fun c -> c#print) (List.sort_uniq comp_atoms atoms_list); *)
		self#iter_atoms (List.sort_uniq comp_atoms atoms_list)

	method to_string = name ^ " - " ^ self#formula (*  ^ "\nAtoms list : \n" ^ String.concat "\n" (List.map (fun c -> c#to_string) atoms_list)  *)
	method equals (molecule:molecule) = name = molecule#name
	method print = print_endline self#to_string
end

class virtual reaction (molecules: molecule list) =
object
	method virtual get_start : (molecule * int) list
	method virtual get_result : (molecule * int) list
	method virtual balance : reaction
	method virtual is_balanced : bool
end