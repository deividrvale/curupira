(*-----------------------------------------------------------------------------
  Abstract name symbols
-----------------------------------------------------------------------------*)

module type NAME = sig
  type t

  val equal : t -> t -> bool
  (** [equal s t] is [true] iff [t] is syntactically equal to [t] *)

  val name_list : unit -> t list

  val of_string : string -> t

  val of_string_opt : string -> t option

  val symbolize : (t -> unit) -> string -> t

  val to_string : t -> string

  val compare : t -> t -> int

  exception Name_Not_found of string
end

module type ATOM = sig
  type t

  val equal : t -> t -> bool

  val atom_list : unit -> t list

  val fresh : unit -> t
end

(*-----------------------------------------------------------------------------
  Functors Implementing Names and Atoms
-----------------------------------------------------------------------------*)

module IndexedNames () : NAME = struct
  exception Name_Not_found of string

  type t = int

  let compare = Int.compare

  let equal = Int.equal

  let names = ref []
  (* [names] is reference pointer to the list of names at certain index it is initialized as the empty list *)

  let names_size = ref 0
  (* [idx_size] is the size of [names] *)

  let name_list () = List.init !names_size Fun.id

  let rec idx_of_name (name : string)
                      (name_lst : string list)
                      (idx : int)
                      : int =
    match name_lst with
    | [] -> idx
    | hd :: tl ->
      if String.equal name hd then
        idx
      else
        idx_of_name name tl (idx + 1)

  let of_string_opt (name : string) =
    let idx = idx_of_name name !names 0 in
    if idx >= !names_size then
      None
    else
      Some (!names_size - 1 - idx)

  let of_string (name : string) =
    match (of_string_opt name) with
    | None ->
      raise (Name_Not_found ("Name: '" ^ name ^ "' is not registered."))
    | Some n -> n

  let symbolize handler new_name =
    match (of_string_opt new_name) with
    | None ->
      let n = !names_size in (
        names_size := n + 1;
        names := new_name :: !names;
        n
      )
    | Some name ->
      handler name;
      name

  let to_string n = List.nth !names (!names_size -1 -n)
end

module NamedAtoms (A : NAME) : ATOM with type t = A.t =
  struct
    type t = A.t

    let equal = A.equal

    let atom_list = A.name_list

    let rec fresh_an n =
      let prefix = "a" in
      let postfix = n + 1 |> Int.to_string in
      A.symbolize
        (fun _ -> let _ = fresh_an (n + 1) in ())
        (prefix ^ postfix)

    let fresh () =
      let all_names = A.name_list () in
      let pretty_names = ["a"; "b"; "c"; "d"; "e"] in
      (* This predicate collect those names in [pretty_names] which are not
      members of [all_names]. *)
      let pred = fun x ->
        let open Utils.Lists in
        not (member String.equal x (List.map A.to_string all_names)) in
      let white_list = List.filter pred pretty_names in
      let n = List.length white_list in
      if (n > 0) then
        let new_name = List.hd white_list in
        A.symbolize (Fun.const ()) new_name
      else
        let n = List.length all_names in
        let prefix = "a" in
        let postfix = n + 1 |> Int.to_string in
        A.symbolize
          (fun _ -> let _ = fresh_an n in ())
          (prefix ^ postfix)
  end
