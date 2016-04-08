let rec draw_42 n d =
	match n with
	| 0 -> d
	| _ -> draw_42 (n - 1) (snd (Deck.drawCard d))

let main () =
	(* test toString *)
	print_endline (String.concat ", " (Deck.toStringVerboseList (Deck.newDeck ()))); print_char '\n';
	print_endline (String.concat ", " (Deck.toStringVerboseList (Deck.newDeck ()))); print_char '\n';
	print_endline (String.concat ", " (Deck.toStringList (Deck.newDeck ()))); print_char '\n';
	(* test draw *)
	print_endline (String.concat ", " (Deck.toStringList (draw_42 42 (Deck.newDeck()))));
	print_endline (String.concat ", " (Deck.toStringList (draw_42 53 (Deck.newDeck()))))
	
let () = main ()