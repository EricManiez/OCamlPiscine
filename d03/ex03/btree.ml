type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_rect x y xsize ysize =
	if xsize > 0 && ysize > 0 then (
			let startx = (x - (xsize / 2)) in
			let starty = (y + (ysize / 2)) in
			Graphics.moveto startx starty;
			Graphics.lineto (startx + (xsize -1)) starty;
			Graphics.lineto (startx + (xsize -1)) (starty - (ysize -1));
			Graphics.lineto (startx) (starty - (ysize -1));
			Graphics.lineto startx starty
	) else ()

let draw_square x y size =
	draw_rect x y size size

let rec size tree =
	match tree with
	| Nil -> 1
	| Node (x, y, z) -> 1 + (size y) + (size z)

let rec height tree =
	match tree with
	| Nil -> 0
	| Node (x, y, z) -> (max (height y) (height z)) + 1
		
let rec draw_tree_node_coord n x y =
	draw_rect (x + 30) y 60 30;
	Graphics.moveto (x + 5) (y - 5);
	match n with
		| Nil -> Graphics.draw_string "Nil"
		| Node (str, n1, n2) ->
			Graphics.draw_string (string_of_int str);
			Graphics.moveto (x + 60) y;
			Graphics.lineto (x + 90) (y + 60);
			draw_tree_node_coord n1 (x + 90) (y + 60);
			Graphics.moveto (x + 60) y;
			Graphics.lineto (x + 90) (y - 60);
			draw_tree_node_coord n2 (x + 90) (y - 60);
			Graphics.moveto (x + 60) y

let draw_tree n =
	draw_tree_node_coord n 10 400

(* -------------------- ex03 --------------------- *)

let rec tree_is_inf v tree =
	match tree with
	| Nil -> true
	| Node (v2, ln, rn) -> v2 < v && tree_is_inf v ln && tree_is_inf v rn

let rec tree_is_sup v tree =
	match tree with
	| Nil -> true
	| Node (v2, ln, rn) -> v2 > v && tree_is_sup v ln && tree_is_sup v rn

let rec is_bst tree =
	match tree with
	| Nil -> true
	| Node (v, ln, rn) -> tree_is_inf v ln && tree_is_sup v rn && is_bst ln && is_bst rn

let rec is_perfect tree =
	is_bst tree && (match tree with
	| Nil -> true
 	| Node (v, Nil, Nil) -> true 
	| Node (v, Node (a, b, c), Node (d, e, f)) -> 
		is_perfect (Node (a, b, c)) && is_perfect (Node (d, e, f)) && height (Node (a, b, c)) == height (Node (d, e, f))
	| _ -> false)

let rec is_balanced tree =
	match tree with
		| Nil -> true
		| Node (v, ln, rn) -> 
			is_bst (Node (v, ln, rn)) &&
			abs ((height ln) - (height rn)) <= 1 &&
			is_balanced ln && is_balanced rn

let rec search_bst n tree =
	match tree with
	| Nil -> false
	| Node (v, ln, rn) -> match n with
		| _ when n = v -> true
		| _ when n < v -> search_bst n ln
		| _ when n > v -> search_bst n rn
		| _ -> false

let rec add_bst n tree =
	match tree with
	| Nil -> Node (n, Nil, Nil)
	| Node (v, ln, rn) -> match n with
		| _ when n = v -> tree (* failwith "value already in binary tree" *)
		| _ when n < v -> Node (v, add_bst n ln, rn)
		| _ when n > v -> Node (v, ln, add_bst n rn)
		| _ -> tree

let rec find_min tree =
	match tree with
	| Node (v, Nil, _) -> v
	| Node (v, ln, _) -> find_min ln
	| _ -> 0

let rec delete_bst n tree =
	match tree with
	| Nil -> tree (* failwith "value not in binary tree" *)
	| Node (v, ln, rn) -> (
		match n with
		| _ when n = v -> (
			match Node (v, ln, rn) with
			| Node (v, Nil, Nil) -> Nil
			| Node (v, ln, Nil) -> ln
			| Node (v, Nil, rn) -> rn
			| Node (v, ln, rn) -> Node (find_min rn, ln, delete_bst (find_min rn) rn)
			| _ -> Nil
		)
		| _ when n < v -> Node (v, delete_bst n ln, rn)
		| _ when n > v -> Node (v, ln, delete_bst n rn)
		| _ -> tree
	)

(* -------------------- TESTS -------------------- *)

let show_us_a_tree tree =
	Graphics.clear_graph ();
	draw_tree tree;
	Graphics.moveto 10 30;
	Graphics.draw_string (Printf.sprintf 
		"Tree size = %d  ||  Tree height = %d || is_bst = %s  ||  is_perfect = %s  ||  is_balanced = %s"
		(size tree) (height tree)
		(string_of_bool (is_bst tree)) (string_of_bool (is_perfect tree)) (string_of_bool (is_balanced tree))
	);
	Graphics.moveto 10 10;
	Graphics.draw_string (Printf.sprintf "serach 5 = %s  ||  search 1 = %s  ||  search 7 = %s || find_min = %s"
		(if is_bst tree then string_of_bool (search_bst 5 tree) else "N/A")
		(if is_bst tree then string_of_bool (search_bst 1 tree) else "N/A")
		(if is_bst tree then string_of_bool (search_bst 7 tree) else "N/A")
		(if is_bst tree then string_of_int (find_min tree) else "N/A")
	);
	if is_bst tree then (
		ignore(Graphics.read_key ());
		Graphics.moveto 320 395;
		Graphics.draw_string " -> add 7 ->";
		draw_tree_node_coord (add_bst 7 tree) 400 400
	);
	if is_bst tree then (
		ignore(Graphics.read_key ());
		Graphics.moveto 700 395;
		Graphics.draw_string "-> delete 5 ->";
		draw_tree_node_coord (delete_bst 5 (add_bst 7 tree)) 800 400
	);

	ignore(Graphics.read_key ());
	()

let main () =
	let tree1 = 
		Node (
			5,
			Node (2, Nil, Nil),
			Node (7, Nil, Nil)
		)
	in
	let tree2 =
		Node (
			1, 
			Node (
				2, 
				Node (3, Nil, Nil)
				, Nil
			), 
			Node (
				4,
				Nil,
				Node (
					5,
					Nil,
					Node (6, Nil, Nil)
				)
			)
		)
	in
	let tree3 =
		Node (
			5,
			Node (
				3,
				Node (1, Nil, Nil),
				Node (6, Nil, Nil)
			),
			Nil
		)
	in
	let tree4 =
		Node (
			5,
			Node (
				3,
				Node (1, Nil, Nil),
				Node (4, Nil, Nil)
			),
			Nil
		)
	in
	let tree5 =
		Node (
			5,
			Node (
				3,
				Node (1, Nil, Nil),
				Node (4, Nil, Nil)
			),
			Node (8, Nil, Nil)
		)
	in
	Graphics.open_graph " 1200x1200";
	show_us_a_tree Nil;
	show_us_a_tree tree1;
	show_us_a_tree tree2;
	show_us_a_tree tree3;
	show_us_a_tree tree4;
	show_us_a_tree tree5

let _ = main ()