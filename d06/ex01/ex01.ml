module StringHash =
	struct
		type t = string
		let equal s1 s2 = s1 = s2
		let hash s = (int_of_char s.[0]) lxor (String.length s)
	end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht