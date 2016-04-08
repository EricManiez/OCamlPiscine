let rot_char n c =
	char_of_int (((int_of_char c) + n) mod 128)

let caesar n str =
	String.map (rot_char n) str

let rot42 str =
	caesar 42 str

let xor_char key c =
	char_of_int (key lxor (int_of_char c))

let xor key str =
	String.map (xor_char key) str

let rec ft_crypt str flist =
	match flist with
	| [] -> str
	| head :: tail -> ft_crypt (head str) tail