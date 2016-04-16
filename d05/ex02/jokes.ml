let () =
	let jokes = Array.make 5 "" in
	Array.set jokes 0 "A pair of jumper cables walks into a bar. The bartender says :\n\"I'll serve you, but don't start anything!\"";
	Array.set jokes 1 "What do you call a guy with a rubber toe?\nRoberto";
	Array.set jokes 2 "What did Snow White say when she came out of the photobooth?\nSomeday my prints will come…";
	Array.set jokes 3 "What do you call the security guards outside of Samsung.\nThe guardians of the galaxy!";
	Array.set jokes 4 "What do you call a computer that sings?\nAdell!";
	Random.self_init ();
	print_endline (Array.get jokes (Random.int 5))