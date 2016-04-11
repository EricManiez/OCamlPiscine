#load "unix.cma"

let my_sleep () = Unix.sleep 1

let main argv =
	let secs = 
		try int_of_string argv.(1) with
		| _ -> 0
	in
	for i = secs downto 1 do
		my_sleep ()
	done

let () =
	let argv = Sys.argv in
	main argv