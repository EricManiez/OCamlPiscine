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

(* -------------------- TESTS -------------------- *)

let print_nucleotide c =
	let nucleotide = generate_nucleotide c in
	Printf.printf ("Nucleotide :\nPhosphate : %s\nDeoxyribose : %s\nNucleobase : %s\n\n") 
		nucleotide.phosphate
		nucleotide.deoxyribose
		(
			match nucleotide.nucleobase with
			| A -> "A"
			| T -> "T"
			| G -> "G"
			| C -> "C"
			| None -> "None"
		)

let () =
	print_nucleotide 'A';
	print_nucleotide 'T';
	print_nucleotide 'G';
	print_nucleotide 'C';
	print_nucleotide 'N'
