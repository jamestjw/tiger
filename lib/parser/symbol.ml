module type SYMBOL = sig
  type symbol

  val to_symbol : string -> symbol
  val name : symbol -> string
  (*
    TODO: Implement lookup tables
    (* Use functors to implement this https://ocaml.org/docs/functors *)
     type 'a table

     val empty : 'a table
     val enter : 'a table * symbol * 'a -> 'a table
     val look : 'a table * symbol -> 'a option *)
end

module Symbol : SYMBOL = struct
  type symbol = string * int

  let sizeHint = 128

  module H = Hashtbl

  let hashtable = H.create sizeHint
  let nextsym = ref 0

  let to_symbol name =
    let i =
      try H.find hashtable name
      with Not_found ->
        let i = !nextsym in
        nextsym := i + 1;
        H.add hashtable name i;
        i
    in
    (name, i)

  (* Just return the first part of the tuple which is the symbol name*)
  let name (s, n) = s
end
