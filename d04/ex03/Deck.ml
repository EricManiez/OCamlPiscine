(* ------------------------------ COLOR ------------------------------ *)

module Color =
struct
	type t = Spade | Heart | Diamond | Club

	let all = [Spade ; Heart ; Diamond ; Club]

	let toString t = 
		match t with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

	let toStringVerbose t = 
		match t with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"
end

(* ------------------------------ VALUE ------------------------------ *)

module Value =
struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

	let toInt t =
		match t with 
		| T2 -> 2
		| T3 -> 3
		| T4 -> 4
		| T5 -> 5
		| T6 -> 6
		| T7 -> 7
		| T8 -> 8
		| T9 -> 9
		| T10 -> 10
		| Jack -> 11
		| Queen -> 12
		| King -> 13
		| As -> 14

	let toString t =
		match t with 
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "J"
		| Queen -> "Q"
		| King -> "K"
		| As -> "A"

	let toStringVerbose t =
		match t with 
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "Jack"
		| Queen -> "Queen"
		| King -> "King"
		| As -> "As"

	let next t =
		match t with 
		| T2 -> T3
		| T3 -> T4
		| T4 -> T5
		| T5 -> T6
		| T6 -> T7
		| T7 -> T8
		| T8 -> T8
		| T9 -> T10
		| T10 -> Jack
		| Jack -> Queen
		| Queen -> King
		| King -> As
		| As -> invalid_arg "function next called with argument As"

	let previous t =
		match t with 
		| T2 -> invalid_arg "function previous called with argument T2"
		| T3 -> T2
		| T4 -> T3
		| T5 -> T4
		| T6 -> T5
		| T7 -> T6
		| T8 -> T7
		| T9 -> T8
		| T10 -> T9
		| Jack -> T10
		| Queen -> Jack
		| King -> Queen
		| As -> King
end

(* ------------------------------ CARD ------------------------------ *)

module Card =
struct
	type t = {value: Value.t; color: Color.t}

	let newCard v c = {value = v; color = c}

	let allSpades = [
		{value = Value.T2 ; color = Color.Spade};
		{value = Value.T3 ; color = Color.Spade};
		{value = Value.T4 ; color = Color.Spade};
		{value = Value.T5 ; color = Color.Spade};
		{value = Value.T6 ; color = Color.Spade};
		{value = Value.T7 ; color = Color.Spade};
		{value = Value.T8 ; color = Color.Spade};
		{value = Value.T9 ; color = Color.Spade};
		{value = Value.T10 ; color = Color.Spade};
		{value = Value.Jack ; color = Color.Spade};
		{value = Value.Queen ; color = Color.Spade};
		{value = Value.King ; color = Color.Spade};
		{value = Value.As ; color = Color.Spade}
	]
	let allHearts = [
		{value = Value.T2 ; color = Color.Heart};
		{value = Value.T3 ; color = Color.Heart};
		{value = Value.T4 ; color = Color.Heart};
		{value = Value.T5 ; color = Color.Heart};
		{value = Value.T6 ; color = Color.Heart};
		{value = Value.T7 ; color = Color.Heart};
		{value = Value.T8 ; color = Color.Heart};
		{value = Value.T9 ; color = Color.Heart};
		{value = Value.T10 ; color = Color.Heart};
		{value = Value.Jack ; color = Color.Heart};
		{value = Value.Queen ; color = Color.Heart};
		{value = Value.King ; color = Color.Heart};
		{value = Value.As ; color = Color.Heart}
	]
	let allDiamonds = [
		{value = Value.T2 ; color = Color.Diamond};
		{value = Value.T3 ; color = Color.Diamond};
		{value = Value.T4 ; color = Color.Diamond};
		{value = Value.T5 ; color = Color.Diamond};
		{value = Value.T6 ; color = Color.Diamond};
		{value = Value.T7 ; color = Color.Diamond};
		{value = Value.T8 ; color = Color.Diamond};
		{value = Value.T9 ; color = Color.Diamond};
		{value = Value.T10 ; color = Color.Diamond};
		{value = Value.Jack ; color = Color.Diamond};
		{value = Value.Queen ; color = Color.Diamond};
		{value = Value.King ; color = Color.Diamond};
		{value = Value.As ; color = Color.Diamond}
	]
	let allClubs = [
		{value = Value.T2 ; color = Color.Club};
		{value = Value.T3 ; color = Color.Club};
		{value = Value.T4 ; color = Color.Club};
		{value = Value.T5 ; color = Color.Club};
		{value = Value.T6 ; color = Color.Club};
		{value = Value.T7 ; color = Color.Club};
		{value = Value.T8 ; color = Color.Club};
		{value = Value.T9 ; color = Color.Club};
		{value = Value.T10 ; color = Color.Club};
		{value = Value.Jack ; color = Color.Club};
		{value = Value.Queen ; color = Color.Club};
		{value = Value.King ; color = Color.Club};
		{value = Value.As ; color = Color.Club}
	]
	let all = [
		{value = Value.T2 ; color = Color.Spade};
		{value = Value.T3 ; color = Color.Spade};
		{value = Value.T4 ; color = Color.Spade};
		{value = Value.T5 ; color = Color.Spade};
		{value = Value.T6 ; color = Color.Spade};
		{value = Value.T7 ; color = Color.Spade};
		{value = Value.T8 ; color = Color.Spade};
		{value = Value.T9 ; color = Color.Spade};
		{value = Value.T10 ; color = Color.Spade};
		{value = Value.Jack ; color = Color.Spade};
		{value = Value.Queen ; color = Color.Spade};
		{value = Value.King ; color = Color.Spade};
		{value = Value.As ; color = Color.Spade};
		{value = Value.T2 ; color = Color.Heart};
		{value = Value.T3 ; color = Color.Heart};
		{value = Value.T4 ; color = Color.Heart};
		{value = Value.T5 ; color = Color.Heart};
		{value = Value.T6 ; color = Color.Heart};
		{value = Value.T7 ; color = Color.Heart};
		{value = Value.T8 ; color = Color.Heart};
		{value = Value.T9 ; color = Color.Heart};
		{value = Value.T10 ; color = Color.Heart};
		{value = Value.Jack ; color = Color.Heart};
		{value = Value.Queen ; color = Color.Heart};
		{value = Value.King ; color = Color.Heart};
		{value = Value.As ; color = Color.Heart};
		{value = Value.T2 ; color = Color.Diamond};
		{value = Value.T3 ; color = Color.Diamond};
		{value = Value.T4 ; color = Color.Diamond};
		{value = Value.T5 ; color = Color.Diamond};
		{value = Value.T6 ; color = Color.Diamond};
		{value = Value.T7 ; color = Color.Diamond};
		{value = Value.T8 ; color = Color.Diamond};
		{value = Value.T9 ; color = Color.Diamond};
		{value = Value.T10 ; color = Color.Diamond};
		{value = Value.Jack ; color = Color.Diamond};
		{value = Value.Queen ; color = Color.Diamond};
		{value = Value.King ; color = Color.Diamond};
		{value = Value.As ; color = Color.Diamond};
		{value = Value.T2 ; color = Color.Club};
		{value = Value.T3 ; color = Color.Club};
		{value = Value.T4 ; color = Color.Club};
		{value = Value.T5 ; color = Color.Club};
		{value = Value.T6 ; color = Color.Club};
		{value = Value.T7 ; color = Color.Club};
		{value = Value.T8 ; color = Color.Club};
		{value = Value.T9 ; color = Color.Club};
		{value = Value.T10 ; color = Color.Club};
		{value = Value.Jack ; color = Color.Club};
		{value = Value.Queen ; color = Color.Club};
		{value = Value.King ; color = Color.Club};
		{value = Value.As ; color = Color.Club}
	]

	let getValue t = t.value
	let getColor t = t.color

	let toString t = Printf.sprintf "%s%s" (Value.toString t.value) (Color.toString t.color)
	let toStringVerbose t = Printf.sprintf "Card (%s, %s)" (Value.toStringVerbose t.value) (Color.toStringVerbose t.color)

	let compare t1 t2 =
		Value.toInt t1.value - Value.toInt t2.value

	let max t1 t2 = 
		let comp = compare t1 t2 in
		match comp with
		| _ when comp < 0 -> t2
		| _ -> t1

	let min t1 t2 = 
		let comp = compare t1 t2 in
		match comp with
		| _ when comp > 0 -> t2
		| _ -> t1

	let best tl =
		match tl with
		| [] -> invalid_arg "function best : list is empty!"
		| head :: tail -> List.fold_left max head tail

	let isOf t c =
		t.color = c

	let isSpade t =
		isOf t Color.Spade

	let isHeart t =
		isOf t Color.Heart

	let isDiamond t =
		isOf t Color.Diamond

	let isClub t =
		isOf t Color.Club
end

(* ------------------------------ DECK ------------------------------ *)

let shuffle d =
	Random.self_init ();
    let tl = List.map (fun c -> (Random.bits (), c)) d in
    let sorted = List.sort compare tl in
    List.map snd sorted

type t = Card.t list

let newDeck () =
	shuffle Card.all

let toStringList d =
	List.map Card.toString d

let toStringVerboseList d =
	List.map Card.toStringVerbose d

let drawCard d =
	match d with
	| [] -> raise (Failure "Cannot draw a card from an empty deck!")
	| head :: tail -> (head, tail)