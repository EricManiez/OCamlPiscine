module OrderedString =
    struct
      type t = string
      let compare x y = compare x y
    end

module StringSet = Set.Make(OrderedString)

let () =
	let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
	StringSet.iter print_endline set;
	print_endline (StringSet.fold ( ^ ) set "")