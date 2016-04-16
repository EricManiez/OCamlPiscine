(*
module Watchtower :
	sig
		type hour = int
		val zero : hour
		val add : hour -> hour -> hour
		val sub : hour -> hour -> hour
	end
*)

type op = Add | Sub

module Watchtower =
	struct
		type hour = int
		let (zero:hour) = (12:hour)
		let add (hour:hour) (time:hour) = (((hour + time) mod zero) : hour)
		let sub (hour:hour) (time:hour) = add zero (hour - time mod zero)
	end

let print_hour one two (op:op) =
	match op with
	| Add -> Printf.printf "%d hours + %d hours = %d hours\n" one two (Watchtower.add one two)
	| Sub -> Printf.printf "%d hours - %d hours = %d hours\n" one two (Watchtower.sub one two)

let () =
	let hour1:Watchtower.hour = 3 in
	let hour2:Watchtower.hour = 6 in
	let hour3:Watchtower.hour = 9 in
	let hour4:Watchtower.hour = Watchtower.zero in
	let hour5:Watchtower.hour = 15 in
	let hour6:Watchtower.hour = 18 in
	
	print_hour hour1 hour2 Add;
	print_hour hour1 hour3 Add;
	print_hour hour1 hour4 Add;
	print_hour hour1 hour5 Add;
	print_hour hour1 hour6 Add;
	print_hour hour1 hour2 Sub;
	print_hour hour1 hour3 Sub;
	print_hour hour1 hour4 Sub;
	print_hour hour1 hour5 Sub;
	print_hour hour1 hour6 Sub;
print_endline "------------------------------------------";
	print_hour hour2 (Watchtower.add hour3 hour4) Add;
	print_hour (Watchtower.add hour2 hour3) hour4 Add;
	print_hour hour2 (Watchtower.sub hour3 hour4) Sub;
	print_hour (Watchtower.sub hour2 hour3) hour4 Sub