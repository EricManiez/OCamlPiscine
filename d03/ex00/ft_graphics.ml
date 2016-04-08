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

let draw_tree_node n =
	draw_tree_node_coord n 10 400

let main () =
	Graphics.open_graph " 800x800";
	draw_square 400 400 200;
	draw_square 0 0 200;
	draw_square (-1) (-1) 200;
	draw_square 400 400 0;
	draw_square 400 400 (-1);
	draw_square 400 400 1;
	Graphics.read_key ();
	draw_tree_node Nil;
	Graphics.read_key ();
	Graphics.clear_graph ();
	draw_tree_node (Node ("hi there", Nil, Nil));
	Graphics.read_key ()

let _ = main ()