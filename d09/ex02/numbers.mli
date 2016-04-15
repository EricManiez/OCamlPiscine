module type MONOID =
	sig
		type element
		val zero1 : element
		val zero2 : element
		val mul : element -> element -> element
		val add : element -> element -> element
		val div : element -> element -> element
		val sub : element -> element -> element
	end

module Calc :
	functor (M : MONOID) ->
		sig
			val add : M.element -> M.element -> M.element
			val sub : M.element -> M.element -> M.element
			val mul : M.element -> M.element -> M.element
			val div : M.element -> M.element -> M.element
			val power : M.element -> int -> M.element
			val fact : M.element -> M.element
		end
