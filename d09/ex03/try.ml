module Try =
	struct
		type 'a t = Success of 'a | Failure of exn

		let return n = Success n
		let bind n f =
			match n with
			| Success (x) -> (
				try f x with
				| (exn) -> Failure exn
			)
			| Failure (x) -> Failure x

		let recover n f =
			match n with
			| Failure (x) -> f x
			| _ -> n

		let filter (n:'a t) (pred:('a -> bool)) =
			match n with
			| Success (x) -> (
				match pred x with
				| false -> Failure x
				| true -> Success x
			)
			| _ -> n
			
		let flatten nn =
			match nn with
			| Success (n) -> (
				match n with
				| Success (x) -> n
				| Failure (x) -> Failure x
			)
			| Failure (n) -> Failure n
	end