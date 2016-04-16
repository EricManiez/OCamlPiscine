let eu_dist arr1 arr2 =
	let acc = ref 0. in
	let upper = min (Array.length arr1) (Array.length arr2) - 1 in
	for i = 0 to upper do
		let a = arr1.(i) in
		let b = arr2.(i) in
		acc := !acc +. ((a *. a) -. (2. *. a *. b) +. (b *. b))
	done;
	sqrt !acc
	
(* -------------------- TESTS -------------------- *)

let print_coordinates arr =
	for i = 0 to Array.length arr - 1 do
		print_float arr.(i);
		print_string "; "
	done
	

let print_result arr1 arr2 =
	print_string "Coordinates for arr1 : [|";
	print_coordinates arr1;
	print_string "|]\nCoordinates for arr2 : [|";
	print_coordinates arr2;
	Printf.printf "|]\nEuclidian distance : %f\n\n" (eu_dist arr1 arr2)

let main () =
	let arr1 = Array.make 2 2. in
	let arr2 = Array.make 2 2. in
	Array.set arr1 1 (-1.);
	Array.set arr2 0 (-2.);
	print_result arr1 arr2;
	let arr1 = Array.make 4 3. in
	let arr2 = Array.make 5 3. in
	Array.set arr1 0 (0.);
	Array.set arr1 2 (4.);
	Array.set arr1 3 (5.);
	Array.set arr2 0 (7.);
	Array.set arr2 1 (6.);
	Array.set arr2 3 (-1.);
	print_result arr1 arr2;
	let arr1 = Array.make 4 0. in
	let arr2 = Array.make 5 0. in
	print_result arr1 arr2;
	let arr1 = Array.make 0 0. in
	let arr2 = Array.make 0 0. in
	print_result arr1 arr2
	

let () = main ()