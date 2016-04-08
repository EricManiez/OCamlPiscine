let print_int_sp n =
	print_int n ; print_char ' '

let main () =
	(* test toString *)
	Printf.printf "test toString :\n%s\n\n" (String.concat ", " (List.map (fun c -> (Value.toString c)) Value.all));
	(* test toStringVerbose *)
	Printf.printf "test toStringVerbose :\n%s\n\n" (String.concat ", " (List.map (fun c -> (Value.toStringVerbose c)) Value.all));
	(* test toInt *)
	Printf.printf "test toInt :\n%s\n\n" (String.concat ", " (List.map (fun c -> string_of_int (Value.toInt c)) Value.all));
	(* test next - error expected *)
	ignore(List.map (fun c -> print_int_sp (Value.toInt (Value.next c))) Value.all);
	(* test previous - error expected *)
	ignore(List.map (fun c -> print_int_sp (Value.toInt (Value.previous c))) (List.rev Value.all)); print_char '\n'
	
let () = main ()