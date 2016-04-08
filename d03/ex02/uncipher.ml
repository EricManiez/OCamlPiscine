let unrot_char n c =
	char_of_int (((int_of_char c) + (128 - n)) mod 128)

let uncaesar n str =
	String.map (unrot_char n) str

let unrot42 str =
	uncaesar 42 str

let xor_char key c =
	char_of_int (key lxor (int_of_char c))

let xor key str =
	String.map (xor_char key) str

let rec ft_uncrypt str flist =
	match flist with
	| [] -> str
	| head :: tail -> ft_uncrypt (head str) tail
