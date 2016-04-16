(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Board.mli                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: gbernard <gbernard@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2016/04/08 19:49:38 by Geoffroy          #+#    #+#             *)
(*   Updated: 2016/04/10 12:19:03 by gbernard         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type t

val newBoard : t
val printBoard : t -> unit
val getCharInCase : t -> int -> int -> int -> char
val getGridOwner : t -> int -> string
val checkWinGrid : t -> int -> bool
val gameWON : t -> bool
val setWinGrid : t -> int -> int -> t
val putTokenInCase : t -> int -> int -> int -> char -> t
