open Syntax.Symbols

module AtomsNames : NAME = IndexedNames ()

module Atoms = NamedAtoms(AtomsNames)

module FPerms = Syntax.Perm.FuncPerm(Atoms)

let () =
  let a = AtomsNames.symbolize (Fun.const ()) "a" in
  let b = AtomsNames.symbolize (Fun.const ()) "b" in
  let c = AtomsNames.symbolize (Fun.const ()) "c" in
  let d = AtomsNames.symbolize (Fun.const ()) "d" in
  let _ = AtomsNames.symbolize (Fun.const ()) "a10" in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let _ = Atoms.fresh () in
  let x = Atoms.fresh () in
  let pId = FPerms.id in
  let swap = fun x -> (
    if Atoms.equal x a
      then b
    else
      if Atoms.equal x b
        then a
      else
        x
  ) in
  ()
  (* let swap_inv = FPerms.inv swap in
  let compute_with_id = pId x in
  print_endline (AtomsNames.to_string @@ swap_inv b);
  print_endline (Utils.Lists.to_string Atoms.to_string (FPerms.get_supp swap_inv));
  print_endline (FPerms.to_string swap) *)
  (* Utils.Lists.print_list AtomsNames.to_string (AtomsNames.name_list ()) *)

let () =
  let open Syntax.Terms in
  let a = AtomsNames.symbolize (Fun.const ()) "a" in
  let b = Atoms.fresh () in
  let atm = NominalTerms.Atm a in
  Utils.Lists.print_list AtomsNames.to_string (Atoms.atom_list ())
  (* print_endline "" *)
