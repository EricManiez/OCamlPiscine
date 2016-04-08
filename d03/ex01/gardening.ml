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
			Graphics.draw_string str;
			Graphics.moveto (x + 60) y;
			Graphics.lineto (x + 90) (y + 60);
			draw_tree_node_coord n1 (x + 90) (y + 60);
			Graphics.moveto (x + 60) y;
			Graphics.lineto (x + 90) (y - 60);
			draw_tree_node_coord n2 (x + 90) (y - 60);
			Graphics.moveto (x + 60) y

let draw_tree n =
	draw_tree_node_coord n 10 400


(* -------------------- TESTS -------------------- *)

let show_us_a_tree tree =
	Graphics.clear_graph ();
	draw_tree tree;
	Graphics.moveto 10 10;
	Graphics.draw_string (Printf.sprintf "Tree size = %d" (size tree));
	Graphics.moveto 150 10;
	Graphics.draw_string (Printf.sprintf "Tree height = %d" (height tree));
	ignore(Graphics.read_key ());
	()

let main () =
	let tree1 = 
		Node (
			"hi there",
			Node (
				"and here",
				Nil,
				Nil
			),
			Node (
				"and there",
				Nil,
				Nil
			)
		)
	in
	let tree2 =
		Node (
			"hi there", 
			Node (
				"and here", 
				Node (
					"and there", 
					Nil, 
					Nil
				)
				, Nil
			), 
			Node (
				"and there",
				Nil,
				Node (
					"and there",
					Nil,
					Node (
						"and there",
						Nil,
						Nil
					)
				)
			)
		)
	in
	let tree3 =
		Node (
			"hi there",
			Node (
				"and here",
				Node (
					"and there",
					Nil,
					Nil
				),
				Nil
			),
			Nil
		)
	in
	Graphics.open_graph " 800x800";
	show_us_a_tree Nil;
	show_us_a_tree tree1;
	show_us_a_tree tree2;
	show_us_a_tree tree3

let _ = main ()