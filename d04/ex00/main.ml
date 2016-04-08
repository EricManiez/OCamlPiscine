let main () =
	ignore(List.map (fun c -> print_endline (Color.toString c)) Color.all); print_char '\n';
	ignore(List.map (fun c -> print_endline (Color.toStringVerbose c)) Color.all)
	
let () = main ()