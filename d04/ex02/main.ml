let print_card c =
	print_endline (Card.toStringVerbose c)

let print_int_sp n =
	print_int n ; print_char ' '

let main () =
	(* test toString *)
	Printf.printf "test toString :\n%s\n\n" (String.concat ", " (List.map Card.toString Card.all));
	(* test toStringVerbose *)
	Printf.printf "test toStringVerbose :\n%s\n\n" (String.concat ", " (List.map Card.toStringVerbose Card.all));
	(* test toInt *)
	Printf.printf "test toInt :\n%s\n\n" (String.concat ", " (List.map (fun c -> string_of_int (Card.toInt c)) Card.all));
	print_endline (Card.toStringVerbose (Card.best Card.all));
	Printf.printf "\ntest isOf Diamond : %s\n" (String.concat ", " (List.map (fun c -> string_of_bool (Card.isOf c Card.Color.Diamond)) Card.all));

	print_endline "-----------------------------------";

	let sample = [
			Card.newCard Card.Value.T7 Card.Color.Spade;
			Card.newCard Card.Value.Queen Card.Color.Diamond;
			Card.newCard Card.Value.Jack Card.Color.Club;
			Card.newCard Card.Value.Queen Card.Color.Heart;
		]
	in
	(* test toString *)
	Printf.printf "test toString : %s\n\n" (String.concat ", " (List.map Card.toString sample));
	(* test toStringVerbose *)
	Printf.printf "test toStringVerbose : %s\n\n" (String.concat ", " (List.map Card.toStringVerbose sample));
	(* test toInt *)
	Printf.printf "test toInt : %s\n\n" (String.concat ", " (List.map (fun c -> string_of_int (Card.toInt c)) sample));
	print_endline (Card.toStringVerbose (Card.best sample));
	Printf.printf "\ntest isOf Diamond : %s\n" (String.concat ", " (List.map (fun c -> string_of_bool (Card.isOf c Card.Color.Diamond)) sample))

	
let () = main ()