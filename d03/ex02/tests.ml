let main () =
	print_endline (Cipher.rot42 " !\"#$%&'");
	print_endline (Uncipher.unrot42 (Cipher.rot42 "abcdefghijklmnop"));
	print_endline (Cipher.rot42 (Uncipher.unrot42 "abcdefghijklmnop"));
	print_endline (Cipher.caesar 42 (Uncipher.uncaesar 42 "abcdefghijklmnop"));
	print_endline (Cipher.xor 42 "abcdefghijklmnop");
	print_endline (Cipher.xor 42 (Uncipher.xor 42 "abcdefghijklmnop"));

	let flist = [Cipher.rot42 ;Cipher.caesar 42 ; Cipher.xor 42] in
	let unflist = [Uncipher.xor 42 ; Uncipher.uncaesar 42 ; Uncipher.unrot42] in
	print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "" flist) unflist);
	print_endline (Cipher.ft_crypt "abcdefghijklmnop" flist);
	print_endline (Cipher.xor 42(Cipher.caesar 42(Cipher.rot42 "abcdefghijklmnop")));
	print_endline (Uncipher.ft_uncrypt "abcdefghijklmnop" unflist);
	print_endline (Uncipher.unrot42 (Uncipher.uncaesar 42 (Uncipher.xor 42 "abcdefghijklmnop")));
	print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "abcdefghijklmnop" flist) unflist);
	print_endline (Uncipher.ft_uncrypt (Cipher.ft_crypt "~!@#$%^&*()_+" flist) unflist)

let _ = main ()