open Syntax.Symbols

module AtomsNames : NAME = IndexedNames ()

module Atoms = NamedAtoms(AtomsNames)

module FPerms = Syntax.Perm.FuncPerm(Atoms)

let () =
  (* let _ = AtomsNames.symbolize (Fun.const ()) "a" in *)
  (* let _ = AtomsNames.symbolize (Fun.const ()) "b" in *)
  (* let _ = AtomsNames.symbolize (Fun.const ()) "c" in *)
  (* let _ = AtomsNames.symbolize (Fun.const ()) "d" in *)
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
  let pId = FPerms.id x in
  (* let compute_with_id = pId x in *)
  Utils.Lists.print_list AtomsNames.to_string (AtomsNames.name_list ())
