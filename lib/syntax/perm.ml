open Symbols

module type PERM =
sig
  type t

  type a

  type supp = a list
  val id : t

  val app : t -> a -> a
  val comp : t -> t -> t
  val inv : t -> t
  val get_supp : t -> supp
  val to_string : t -> string
end

module type FuncPerm =
  functor (A : ATOM) -> PERM with type a = A.t and type t = A.t -> A.t

module FuncPerm : FuncPerm = functor (A : ATOM) -> struct
  type t = A.t -> A.t
  type a = A.t

  type supp = a list

  let id = Fun.id

  let app p a = p a

  let comp = Fun.compose

  let inv p =
    let open Utils.Lists in
    let domain = A.atom_list () in
    let range = List.map p domain in
    let inv_pairs = List.combine range domain in
    map_from_assoc_list A.equal inv_pairs

  let get_supp p =
    let pred = fun a -> not (A.equal (p a) a) in
    List.filter pred (A.atom_list ())

  let to_string p =
    let open Utils.Lists in
    let supp = get_supp p in
    let range = List.map p supp in
    let pairs = (List.combine supp range) in
    let print_pairs =
      (fun x -> "(" ^ A.to_string (fst x) ^ "," ^ A.to_string (snd x) ^ ")") in
    (* to_string print_pairs pairs *)
    if List.length supp == 2 then
      print_pairs (List.hd pairs)
    else
      to_string print_pairs pairs
end
