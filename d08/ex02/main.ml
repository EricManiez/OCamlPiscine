let arcane_list = [
	new Alkanes.methane ;
	new Alkanes.ethane ;
	new Alkanes.alkane 3;
	new Alkanes.alkane 4;
	new Alkanes.pentane ;
	new Alkanes.alkane 6;
	new Alkanes.alkane 7;
	new Alkanes.octane ;
	new Alkanes.alkane 9;
	new Alkanes.alkane 10;
	new Alkanes.alkane 11;
	new Alkanes.alkane 12;
	new Alkanes.alkane 13;
	new Alkanes.alkane 14;
	new Alkanes.alkane 15;
	new Alkanes.hexadecane ;
	new Alkanes.alkane 17;
	new Alkanes.alkane 18;
	new Alkanes.alkane 19;
	new Alkanes.icosane;
	new Alkanes.triacontane ;
	new Alkanes.tetracontane ;
	new Alkanes.pentacontane ;
	new Alkanes.hexacontane ;
]

let test_molecule molecule =
	Random.self_init ();
	let rand = List.nth arcane_list (Random.int (List.length arcane_list)) in
	Printf.printf "%s\nThis molecule is equals to %s : %B\n----------\n" molecule#to_string rand#name (molecule#equals rand)

let () =
	(* List.iter test_atom atom_list *)
	List.iter test_molecule arcane_list
	