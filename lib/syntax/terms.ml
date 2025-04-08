open Perm
open Symbols

module type SYNTAX = sig
  type atm (* atoms     *)
  type var (* variables *)
  type fct (* function symbols *)
  type pmt (* permutation *)

  type s = fct -> int (* signature *)

  type term =
  | Atm of atm
  | Sus of pmt * var
  | Abs of atm * term
  | Fct of s * term list

end

module type NOM =
  functor (A : ATOM)
          (V : NAME)
          (F : NAME)
          (P : PERM)
          ->
            SYNTAX
              with type atm = A.t and
                   type var = V.t and
                   type fct = F.t and
                   type pmt = P.t

module Make : NOM =
  functor (A : ATOM)
          (V : NAME)
          (F : NAME)
          (P : PERM) ->
  struct
    type atm = A.t
    type var = V.t
    type fct = F.t
    type pmt = P.t

    type s = fct -> int (*  *)

    let f (x : atm) =
      A.equal x x

    type term =
    | Atm of atm
    | Sus of pmt * var
    | Abs of atm * term
    | Fct of s * term list
  end

(* Implementations *)

(* Names *)
module AtomsNames      : NAME = IndexedNames ()
module Variables       : NAME = IndexedNames ()
module FunctionSymbols : NAME = IndexedNames ()

(* Atoms *)
module Atoms = Symbols.NamedAtoms(AtomsNames)

(* Permutations *)
module P = FuncPerm(Atoms)

module NominalTerms = Make(Atoms)(Variables)(FunctionSymbols)(P)
