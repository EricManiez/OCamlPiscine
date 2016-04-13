module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS =
	sig
		val bits : int		
	end

module type MAKE =
	functor (B : FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
	functor (B : FRACTIONNAL_BITS) ->
	struct
		type t = int
		let shift = B.bits
		let mask = ((1 lsl shift) - 1)
		
		let of_int x = x lsl shift
		let of_float x = 
			let b_unit = (int_of_float x) lsl shift in
			let decimal = x -. float_of_int (int_of_float x) in
			let decimal_pow = int_of_float (decimal *. float_of_int (1 lsl shift)) in
			b_unit + decimal_pow
		let to_float x = 
			let decimal = (float_of_int (x land mask) /. float_of_int (1 lsl shift)) in
			float_of_int (x lsr shift) +. decimal
		let to_int x = x lsr shift
		let to_string x = string_of_float (to_float x)
		
		let zero = 0
		let one = 1 lsl B.bits
		
		let succ x = x + of_int 1
		let pred x = x - of_int 1
		
		let min x y = min x y
		let max x y = max x y
		
		let gth x y = x > y
		let lth x y = x < y
		let gte x y = x >= y
		let lte x y = x <= y
		let eqp x y = x == y
		let eqs x y = x = y
		
		let add x y = of_float (to_float x +. to_float y)
		let sub x y = of_float (to_float x -. to_float y)
		let mul x y = of_float (to_float x *. to_float y)
		let div x y = of_float (to_float x /. to_float y)
		
		let foreach lower upper f =
			for i = lower to upper do
				f i
			done
	end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
	Printf.printf "x > y => %s\n" (string_of_bool (Fixed8.gth x8 y8));
	Printf.printf "x = y => %s\n" (string_of_bool (Fixed8.eqs x8 y8));
	Printf.printf "x < y => %s\n" (string_of_bool (Fixed8.lth x8 y8));
	Printf.printf "max x y => %f\n" (Fixed8.to_float (Fixed8.max x8 y8));
	Printf.printf "y - x = %f\n" (Fixed8.to_float (Fixed8.sub y8 x8));
	Printf.printf "pred x =  %f ||  x = %f  ||  succ x = %f\n" (Fixed8.to_float (Fixed8.pred x8)) (Fixed8.to_float x8) (Fixed8.to_float (Fixed8.succ x8))
