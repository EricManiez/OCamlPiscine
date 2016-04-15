let atom_list = [
		(new Atoms.hydrogen);
		(new Atoms.helium);
		(new Atoms.lithium);
		(new Atoms.beryllium);
		(new Atoms.boron);
		(new Atoms.carbon);
		(new Atoms.nitrogen);
		(new Atoms.oxygen);
		(new Atoms.fluorine);
		(new Atoms.neon);
		(new Atoms.sulfur);
	]

let test_atom atom =
	Random.self_init ();
	let rand = List.nth atom_list (Random.int (List.length atom_list)) in
	Printf.printf "----------\n%s\nThis atom is equals to %s : %B\n----------\n" atom#to_string rand#name (atom#equals rand)

let () =
	List.iter test_atom atom_list