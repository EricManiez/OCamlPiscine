(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Board.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gbernard <gbernard@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2016/04/08 21:00:20 by Geoffroy          #+#    #+#             *)
(*   Updated: 2016/04/10 12:21:01 by gbernard         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)


module Grid = struct

	type t = {
		grid 	: 	char list;
		owner 	:	string;
		num 	:	int
	}

	let newGrid n =
		{
			grid = ['-'; '-'; '-'; '-'; '-'; '-'; '-'; '-'; '-'];
			owner = "None";
			num = n
		}

	let getGrid t =
		match t with
		| { grid = gr; owner = ow; num = n}		-> gr

	let rec checkIsEmpty lst =
		match lst with
		| []			-> false
		| first::next	-> if first = '-' then true else checkIsEmpty next


	let checkFull t =
		match t with
		| { grid = gr; owner = ow; num = n}		-> if checkIsEmpty gr then false else true

	let getRow t n =
		let gr = getGrid t in
		match gr with
		| one::two::three::four::five::six::seven::eight::nine::ten	-> 	begin
																		match n with
																		| 1		-> (one, two, three)
																		| 2		-> (four, five, six)
																		| 3		-> (seven, eight, nine)
																		| _		-> ('w', 'w', 'w')
																		end
		| _															->	('w', 'w', 'w')

	let getCol t n =
		let gr = getGrid t in
		match gr with
		| one::two::three::four::five::six::seven::eight::nine::ten	-> 	begin
																		match n with
																		| 1		-> (one, four, seven)
																		| 2		-> (two, five, eight)
																		| 3		-> (three, six, nine)
																		| _		-> ('w', 'w', 'w')
																		end
		| _															->	('w', 'w', 'w')

	let getUpperLeftDiag t =
		let gr = getGrid t in
		match gr with
		| one::two::three::four::five::six::seven::eight::nine::ten	-> 	(one, five, nine)
		| _															->	('w', 'w', 'w')

	let getUpperRightDiag t =
		let gr = getGrid t in
		match gr with
		| one::two::three::four::five::six::seven::eight::nine::ten	-> 	(three, five, seven)
		| _															->	('w', 'w', 'w')

	let getCharAt g r c =
		match g with
		| { grid = gr; owner = ow; num = n}		->	let num = ((((r - 1) * 3) + c) - 1) in
													List.nth gr num

	let getOwner greed =
	match greed with
	| {grid = g; owner = o; num = n}	-> o

	let checkDiags t =
		let diag1 = getUpperRightDiag t in
		let diag2 = getUpperLeftDiag t in
		match diag1 with
		|  ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
		| _ 			->  match diag2 with
							|  ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
							| _ 									-> false

	let checkCols t =
		let col1 = getCol t 1 in
		let col2 = getCol t 2 in
		let col3 = getCol t 3 in
		match col1 with
		| ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
		| _				->	match col2 with
							| ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
							| _				->	match col3 with
												| ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
												| _									-> false

	let checkRows t =
		let row1 = getRow t 1 in
		let row2 = getRow t 2 in
		let row3 = getRow t 3 in
		match row1 with
		| ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
		| _				->	match row2 with
							| ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
							| _				->	match row3 with
												| ('O', 'O', 'O') | ('X', 'X', 'X')	-> true
												| _									-> false

	let switchChar lst r ch =
	let rec loop lst ite ret =
		match lst with
		| []		->	ret
		| hd::tl 	->	if ite = r then (ret @ [ch] @ tl)
						else loop tl (ite + 1) (ret @ [hd])
	in loop lst 1 []

	let setCharAt g r ch =
		match g with
		| { grid = gr; owner = ow; num = n}		-> { grid = (switchChar gr r ch); owner = ow; num = n}

	let setWinGrid g pl =
		match g with
		| { grid = gr; owner = ow; num = n}		->	match pl with
													| 2	-> {grid = ['\\'; ' '; '/'; ' '; 'X'; ' '; '/'; ' '; '\\']; owner = "p2"; num = n}
													| 1	-> {grid = ['/'; '-'; '\\'; '|'; ' '; '|'; '\\'; '_'; '/']; owner = "p1"; num = n}
													| _ -> g

end

(************************** BOARD ***************************)
type t = Grid.t list

let newBoard  =
let rec loop ite board =
	match ite with
	| 10	 	->  board
	| _ 		->	loop (ite + 1) (board @ [(Grid.newGrid ite)])
in loop 2 [(Grid.newGrid 1)]

let switchGrid lst r g =
	let rec loop lst ite ret =
		match lst with
		| []		->	ret
		| hd::tl 	->	if ite = r then (ret @ [g] @ tl)
						else loop tl (ite + 1) (ret @ [hd])
in loop lst 1 []

let getGridNum n board =
	let rec loop ite lst =
		match lst with
		| []			-> Grid.newGrid 0
		| first::next	-> if ite = n then first else (loop (ite + 1) next)
	in loop 1 board

let getCharInCase board g r c =
	let grid = getGridNum g board in
	Grid.getCharAt grid r c

let putTokenInCase board gr row col ch =
	let modGrid = (Grid.setCharAt (getGridNum gr board) (((row - 1) * 3) + col) ch) in
	switchGrid board gr modGrid

let getGridOwner board gr =
	let greed = getGridNum gr board in
	Grid.getOwner greed

let checkWinGrid board gr =
	let greed = (getGridNum gr board) in
	if (Grid.checkFull greed) || (Grid.checkRows greed) || (Grid.checkCols greed) || (Grid.checkDiags greed) then true
	else false

let gameWonRows board =
	match board with
	| one::two::three::four::five::six::seven::eight::nine::tail		->
	if ((Grid.getOwner one <> "None") && (Grid.getOwner one) = (Grid.getOwner two) && (Grid.getOwner two) = (Grid.getOwner three)) ||
	 	((Grid.getOwner four <> "None") && (Grid.getOwner four) = (Grid.getOwner five) && (Grid.getOwner five) = (Grid.getOwner six)) ||
		((Grid.getOwner seven <> "None") && (Grid.getOwner seven) = (Grid.getOwner eight) && (Grid.getOwner eight) = (Grid.getOwner nine))
	then true
	else false
	| _			-> false

let gameWonCols board =
	match board with
	| one::two::three::four::five::six::seven::eight::nine::tail		->
	if ((Grid.getOwner one <> "None") && (Grid.getOwner one) = (Grid.getOwner four) && (Grid.getOwner four) = (Grid.getOwner seven)) ||
	 	((Grid.getOwner two <> "None") && (Grid.getOwner two) = (Grid.getOwner five) && (Grid.getOwner five) = (Grid.getOwner eight)) ||
		((Grid.getOwner three <> "None") && (Grid.getOwner three) = (Grid.getOwner six) && (Grid.getOwner six) = (Grid.getOwner nine))
	then true
	else false
	| _			-> false

let gameWonDiags board =
	match board with
	| one::two::three::four::five::six::seven::eight::nine::tail		->
	if ((Grid.getOwner one <> "None") && (Grid.getOwner one) = (Grid.getOwner five) && (Grid.getOwner five) = (Grid.getOwner nine)) ||
	 	((Grid.getOwner three <> "None") && (Grid.getOwner three) = (Grid.getOwner five) && (Grid.getOwner five) = (Grid.getOwner seven))
	then true
	else false
	| _			-> false

let gameIsFull board =
	match board with
	| one::two::three::four::five::six::seven::eight::nine::tail		->
	if ((Grid.getOwner one <> "None") && (Grid.getOwner two <> "None") && (Grid.getOwner three <> "None") &&
		(Grid.getOwner four <> "None") && (Grid.getOwner five <> "None") && (Grid.getOwner six <> "None") &&
		(Grid.getOwner seven <> "None") && (Grid.getOwner eight <> "None") && (Grid.getOwner nine <> "None"))
	then true
	else false
	| _			-> false

let gameWON board =
	if ((gameIsFull board) || (gameWonDiags board) || (gameWonCols board) || (gameWonRows board))
	then true
	else false

let setWinGrid board gr pl =
	let greed = (getGridNum gr board) in
	switchGrid board gr (Grid.setWinGrid greed pl)

(******************** DISPLAY **************************)
let printGridline line =
	match line with
	| (one, two, three)			-> 	print_char one;
									print_char ' ';
									print_char two;
									print_char ' ';
									print_char three

let printDelim () =
	print_endline "\n----------------------"

let printBoard board =
	let rec loop line col orig num =
	printGridline (Grid.getRow (getGridNum num board) line);
	if (col = 3 && line < 3) then
		begin
		print_char '\n';
		loop (line + 1) 1 orig orig
		end
	else if (col = 3 && line = 3 && num < 9) then
		begin
		printDelim ();
		loop 1 1 (orig + 3) (orig + 3)
		end
	else if (col = 3 && line = 3 && num = 9) then
		begin
		print_string "\n\n"
		end
	else
		begin
		print_string " | ";
		loop line (col + 1) orig (num + 1)
		end
	in loop 1 1 1 1

(*

	- - - | - - - | - - -
	- - - | - - - | - - -
	- - - | - - - | - - -
	---------------------
	- - - | - - - | - - -
	- - - | - - - | - - -
	- - - | - - - | - - -
	---------------------
	- - - | - - - | - - -
	- - - | - - - | - - -
	- - - | - - - | - - -

*)
