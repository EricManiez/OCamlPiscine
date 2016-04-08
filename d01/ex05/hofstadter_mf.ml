let rec hfs_m n =
	if n < 0 then (-1)
	else if n = 0 then 0
	else n - (hfs_f (hfs_m (n - 1)))

and hfs_f n =
	if n < 0 then (-1)
	else if n = 0 then 1
	else n - (hfs_m (hfs_f (n - 1)))

(* -------------------- TESTS -------------------- *)

let print_result_m n = 
	print_string "hfs_m (";
	print_int n;
	print_string ") = ";
	print_int (hfs_m n);
	print_char '\n'

let print_result_f n = 
	print_string "hfs_f (";
	print_int n;
	print_string ") = ";
	print_int (hfs_f n);
	print_char '\n'

let main () =
	print_result_m (-1);
	print_result_m (0);
	print_result_m 1;
	print_result_m 4;
	print_result_m 6;
	print_result_m 8;
	print_result_f (-1);
	print_result_f (0);
	print_result_f 1;
	print_result_f 4;
	print_result_f 6;
	print_result_f 8

let () = main ()