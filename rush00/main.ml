(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gbernard <gbernard@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2016/04/08 20:59:07 by Geoffroy          #+#    #+#             *)
(*   Updated: 2016/04/10 12:33:34 by gbernard         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)



(*							COLORS DEFINES									  *)

let col_reset =				"\027[0m"
let col_boldBlack =			"\027[1m\027[30m"
let col_boldRed =   		"\027[1m\027[31m"
let col_boldGreen = 		"\027[1m\027[32m"
let col_boldYellow =		"\027[1m\027[33m"
let col_boldBlue =  		"\027[1m\027[34m"
let col_boldMagenta	=		"\027[1m\027[35m"
let col_boldCyan  = 		"\027[1m\027[36m"
let col_boldWhite = 		"\027[1m\027[37m"
(******************************************************************************)

let print_color col str =
	print_endline (col ^ str ^ col_reset)

(******************************* GAME UTILS ***********************************)
type game = {
	p1Name	: string;
	p2Name	: string;
	p1Token : char;
	p2Token : char;
	p1Score : int;
	p2Score : int;
	board : Board.t;
	turn : int
}

let getFreshGame = { 	p1Name = "Player 1"; p2Name = "Player 2";
						p1Token = 'O'; p2Token = 'X';
						p1Score = 0; p2Score = 0;
						board = Board.newBoard; turn = 1
					}

let getBoard game =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd ; turn = tu}					->	bd

let getTurn game =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd ; turn = tu}					->	tu

let getPlayerName game n =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd ; turn = tu}				->	match n with
												| 1		-> p1
												| 2		-> p2
												| _		-> "Unknown"

let getPlayerToken game n =
match game with
| { p1Name = p1; p2Name = p2;
	p1Token = t1; p2Token = t2;
	p1Score = s1; p2Score = s2;
	board = bd ; turn = tu}				->	match n with
											| 1		-> t1
											| 2		-> t2
											| _		-> 'W'

let setBoard game nb =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd; turn = tu}					->	{p1Name = p1; p2Name = p2; p1Token = t1; p2Token = t2;
														p1Score = s1 ; p2Score = s2; board = nb; turn = tu}


let setPlayer1 game =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd; turn = tu}					->	print_color col_boldWhite "Enter Player 1 name : ";
												let str1 = read_line () in
												{ p1Name = str1; p2Name = p2;
													p1Token = t1; p2Token = t2;
													p1Score = s1; p2Score = s2; board = bd; turn = tu}

let setPlayer2 game =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd; turn = tu}					->	print_color col_boldWhite "Enter Player 2 name : ";
												let str2 = read_line () in
												{ p1Name = p1; p2Name = str2;
												p1Token = t1; p2Token = t2;
												p1Score = s1; p2Score = s2; board = bd; turn = tu}

let switchTurn game =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd; turn = tu}					->	match tu with
													| 1				-> { p1Name = p1; p2Name = p2;
																		p1Token = t1; p2Token = t2;
																		p1Score = s1; p2Score = s2; board = bd; turn = 2}
													| 2				-> { p1Name = p1; p2Name = p2;
																		p1Token = t1; p2Token = t2;
																		p1Score = s1; p2Score = s2; board = bd; turn = 1}
													| _				->	{ p1Name = p1; p2Name = p2;
																		p1Token = t1; p2Token = t2;
																		p1Score = s1; p2Score = s2; board = bd; turn = 2}

let dispScores game =
	match game with
	| { p1Name = p1; p2Name = p2;
		p1Token = t1; p2Token = t2;
		p1Score = s1; p2Score = s2;
		board = bd; turn = tu}					-> 	print_color col_boldYellow (p1 ^ " : " ^ col_boldWhite ^ (string_of_int s1));
												print_color col_boldYellow (p2 ^ " : " ^ col_boldWhite ^ (string_of_int s2))


(******************************************************************************)



(****************************** PROG STATES ***********************************)

let is_digit c =
	(c >= '0' && c <= '9')

let ft_string_all f str =
	let rec checkit a =
		if a > 0 && f (String.get str a) then
			checkit (a - 1)
		else
			f (String.get str a);
	in
	if ((String.length str) - 1) >= 0 then
		checkit ((String.length str) - 1)
	else
		false


let strSplit str =
	let rec loop mimile nb ret =
		let ori = String.trim mimile in
		let lgth = String.length ori in
		let cont = String.contains ori ' ' in
		match lgth with
		| 0				-> print_endline "Empty Line"; ret
		| _				-> 	match cont with
							| true			->	let index = String.index ori ' ' in
												let sub = String.sub ori 0 index in
												let next = String.sub ori (index + 1) (lgth - index - 1) in
												begin
												match nb with
												| 0 | 1		-> 	loop next (nb + 1) (ret @ [sub])
												| _			->	print_color col_boldRed "Too many parameters"; []
												end
							| false			->	let rest = String.sub ori 0 lgth in
												match nb with
												| 0 | 1		->	print_color col_boldRed "Not enought parameters"; []
												| 2			->	(ret @ [rest])
												| _			->	print_endline "Not used"; []
	in loop str 0 []


let quitProg () =
	print_color col_boldGreen "\n*** See ya ***"

let getMainMenuInput () =
	print_string "type in your command: ";
	read_line ()

let getGameInput () =
	read_line ()

let print_turn game =
	print_color col_boldWhite ((getPlayerName game (getTurn game)) ^ " turn to play :")

let checkInput str =
	let cmdList = strSplit str in
	match cmdList with
	| first::sec::third::tail 			->	let f = ft_string_all is_digit first in
											let s = ft_string_all is_digit sec in
											let t = ft_string_all is_digit third in
											let res = (f, s, t) in
											begin
											match res with
											| (true, true, true)				-> (int_of_string first, int_of_string sec, int_of_string third)
											| (_, _, _)							-> (0,0,0)
											end
	| _									-> (0,0,0)

let checkValid game cmd =
	match cmd with
	| (fst, snd, thd)
	when fst > 0 && fst < 10 && snd > 0 && snd < 4	&& thd > 0 && thd < 4 && (Board.getGridOwner (getBoard game) fst) = "None"
			-> true
	| _		-> false

let checkWinGrid game gr =
	let isWin = Board.checkWinGrid (getBoard game) gr in
	match isWin with
	| false -> (getBoard game)
	| true	-> print_endline ("Grid " ^ (string_of_int gr) ^ " won !"); (Board.setWinGrid (getBoard game) gr (getTurn game))

let putToken game gr row col =
	let boardTokenPlayed = (Board.putTokenInCase (getBoard game) gr row col (getPlayerToken game (getTurn game))) in
	let gameTokenPlayed = (setBoard game boardTokenPlayed) in
	(checkWinGrid gameTokenPlayed gr)

let dispWinner game =
	let turn = getTurn game in
	match turn with
	| 1				-> print_color col_boldGreen ("Congratulations ! " ^ (getPlayerName game 2) ^ " wins the game !")
	| 2				-> print_color col_boldGreen ("Congratulations ! " ^ (getPlayerName game 1) ^ " wins the game !")
	| _				-> ()

let rec turn game =
		Board.printBoard (getBoard game);
		if Board.gameWON (getBoard game) then dispWinner game
		else
		begin
		print_turn game;
		let str = getGameInput () in
		begin
		match str with
		| "quit" | "exit" | "q"		-> ()
		| _							->
		let map = checkInput str in
		let res = checkValid game map in
		begin
		match res with
		| false						-> print_color col_boldRed "Invalid Move"; turn game
		| true						->	begin
										match map with
										| (fst, snd, thd)			->	begin
																		let ch = Board.getCharInCase (getBoard game) fst snd thd in
																		match ch with
																		| '-'			->	turn (switchTurn(setBoard game (putToken game fst snd thd)))
																		| _				->	print_color col_boldRed "Invalid Move"; turn game
																		end
										end
		end
	end
	end

let dispRules () =
	print_color col_boldWhite "Board Pattern\n";
	print_endline " ------    ------    ------";
	print_endline "|      |  |      |  |      |";
	print_endline "|   1  |  |   2  |  |   3  |";
	print_endline "|      |  |      |  |      |";
	print_endline " ------    ------    ------ ";
	print_endline "                            ";
	print_endline " ------    ------    ------";
	print_endline "|      |  |      |  |      |";
	print_endline "|   4  |  |   5  |  |   6  |";
	print_endline "|      |  |      |  |      |";
	print_endline " ------    ------    ------ ";
	print_endline "                            ";
	print_endline " ------    ------    ------";
	print_endline "|      |  |      |  |      |";
	print_endline "|   7  |  |   8  |  |   9  |";
	print_endline "|      |  |      |  |      |";
	print_endline " ------    ------    ------ ";
	print_endline "\n";
	print_color col_boldWhite "Input to play is : [grid number] [row] [col]\n"

let startGame game =
	dispRules ();
	print_color col_boldYellow "\n*** Game Starting ***\n\n";
	turn game

let rec mainMenu game =
	let str = getMainMenuInput () in
	match str with
	| "quit"	| "q" | "exit" 			->	quitProg ()
	| "start"							->	startGame game
	| "print"							->	Board.printBoard (getBoard game); mainMenu game
	| "set player 1" | "set 1"			->	mainMenu (setPlayer1 game)
	| "set player 2" | "set 2"			->	mainMenu (setPlayer2 game)
	| "scores"							->	dispScores game ; mainMenu game
	| _ 								-> 	print_color col_boldYellow "invalid command";
											mainMenu game


let main () =
	print_color col_boldGreen "\n*** Welcome to Tic Tac Toe Camel ***\n";
	mainMenu getFreshGame

(******************************************************************************)
let () = main ()
