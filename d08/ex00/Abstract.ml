class virtual atom (n:string) (sym:string) (a_n:int) =
object (self)
	val name = n
	val symbol = sym
	val atomic_number = a_n

	method name = name
	method symbol = symbol
	method atomic_number = atomic_number

	method to_string = name ^ " - " ^ symbol ^ " - " ^ string_of_int atomic_number
	method equals (atom:atom) = atomic_number = atom#atomic_number
	method print = print_endline self#to_string
end