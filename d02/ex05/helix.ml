(* -------------------- ex04 -------------------- *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | G | C | None

type nucleotide = {
	phosphate : phosphate;
	deoxyribose : deoxyribose;
	nucleobase : nucleobase
}

let generate_nucleotide c =
	let nucleotide = {
		phosphate = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase = match c with
		| 'A' -> A
		| 'T' -> T
		| 'G' -> G
		| 'C' -> C
		| _ -> None
	} in
	nucleotide

(* -------------------- ex05 -------------------- *)

type helix = nucleotide list

let string_of_nucleobase n =
	match n with
	| A -> "A"
	| T -> "T"
	| G -> "G"
	| C -> "C"
	| None -> "None"

let rec generate_helix n =
	match n with
	| _ when n < 1 -> []
	| _ ->
		Random.self_init ();
		[generate_nucleotide (
			match Random.int 4 with
			| 0 -> 'A'
			| 1 -> 'T'
			| 2 -> 'G'
			| 3 -> 'C'
			| _ -> 'X'
		)] @ generate_helix (n -1)

let rec helix_to_string h =
	match h with
	| [] -> ""
	| head :: tail -> (string_of_nucleobase head.nucleobase) ^ helix_to_string tail

let rec complementary_helix h =
	match h with
	| [] -> []
	| head :: tail -> [generate_nucleotide (
		match head.nucleobase with
		| A -> 'T'
		| T -> 'A'
		| C -> 'G'
		| G -> 'C'
		| _ -> 'X'
	)] @ (complementary_helix tail)

(* -------------------- TESTS -------------------- *)

let print_nucleotide n =
	Printf.printf ("Phosphate : %s\nDeoxyribose : %s\nNucleobase : %s\n\n") 
		n.phosphate
		n.deoxyribose
		(
			match n.nucleobase with
			| A -> "A"
			| T -> "T"
			| G -> "G"
			| C -> "C"
			| None -> "None"
		)

let test_helix n =
	let hel = generate_helix n in
	Printf.printf ("Helix - %d\n\n") n;
	List.map print_nucleotide hel;
	Printf.printf "Helix :               %s\n" (helix_to_string hel);
	Printf.printf "Complimentary helix : %s\n"(helix_to_string (complementary_helix hel));
	print_endline ("\n----------------------------------------\n")

let () =
	test_helix (-1);
	test_helix 0;
	test_helix 1;
	test_helix 5