(* 
module App :
	sig
		type project = string * string * int
		val zero : project
		val combine : project -> project -> project
		val fail : project -> project
		val success : project -> project
	end
 *)

 module App =
 	struct
 		type project = string * string * int
 		
 		let zero = (("", "", 0):project)
 		
 		let combine ((n1, s1, g1):project) ((n2, s2, g2):project) = 
 			let avg = (g1 + g2) / 2 in
 			let status = if avg >= 80 then "succeed" else "failed" in
 			let name = n1 ^ n2 in
 			((name, status, avg):project)

 		let fail ((n, _, g):project) = combine (n, "", g) ("", "", -g)
 		let success ((n, _, g):project) = combine (n, "", g) ("", "", 160 - g)
 	end

let () =
	let print_proj ((n, s, g):App.project) = Printf.printf "Project name : %s  ||  Status : %s  ||  Grade : %d\n" n s g in
	let (p1:App.project) = ("Piscine Ocaml.", "succeed", 9001) in
	let (p2:App.project) = ("Piscine CPP.", "succeed", 80) in
	let (p3:App.project) = ("Piscine PHP.", "failed", -9000) in
	print_proj App.zero;
	print_proj (App.fail p1);
	print_proj (App.fail p2);
	print_proj (App.fail p3);
	print_proj (App.success p1);
	print_proj (App.success p2);
	print_proj (App.success p3);
	print_proj p1;
	print_proj p2;
	print_proj p3;
	print_proj (App.combine p1 p2);
	print_proj (App.combine p3 p2);
	print_proj (App.combine p1 p3);
print_endline "------------------------------------"