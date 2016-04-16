let molecule_list = [
	new Molecules.water;
	new Molecules.carbon_dioxyde;
	new Molecules.ethanol;
	new Molecules.polyethylene;
	new Molecules.methane;
	new Molecules.methanethiol;
	new Molecules.sulfuric_acid;
	new Molecules.megaphone;
	new Molecules.trinitrotoluene;
	new Molecules.lithium_fluoride;
	new Molecules.dilithium_peroxide;
	new Molecules.lithium_hydride;
	new Molecules.boric_acid;
	new Molecules.boron_carbide;
]

let test_molecule molecule =
	Random.self_init ();
	let rand = List.nth molecule_list (Random.int (List.length molecule_list)) in
	Printf.printf "----------\n%s\nThis molecule is equals to %s : %B\n----------\n" molecule#to_string rand#name (molecule#equals rand)

let () =
	(* List.iter test_atom atom_list *)
	List.iter test_molecule molecule_list
	