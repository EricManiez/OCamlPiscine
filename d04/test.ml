let my_sleep () = Unix.sleep 1

let main argv =
	let sec = int_of_string argv.(1) in
	for i = sec downto 1 do
		my_sleep ()
	done

let () =
	let argv = Sys.argv in
	main argv
