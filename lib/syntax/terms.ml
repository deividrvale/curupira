open Perm
open Symbols

module type SYNTAX = sig
  type a (* atoms *)
  type v (* variables *)
  type f (* function symbols *)
  type p (* permutation *)

  type s = f * int (*  *)

  type term =
  | Atm of a
  | Sus of p * v
  | Abs of a * term
  | Fct of s * term list

end

module type NOM =
  functor (A : ATOM)
          (V : NAME)
          (F : NAME)
          (P : PERM) ->
            SYNTAX
              with type a = A.t and
                   type v = V.t and
                   type f = F.t and
                   type p = P.t

module Make : NOM =
  functor (A : ATOM)
          (V : NAME)
          (F : NAME)
          (P : PERM) ->
  struct
    type a = A.t
    type v = V.t
    type f = F.t
    type p = P.t

    type s = f * int (*  *)

    type term =
    | Atm of a
    | Sus of p * v
    | Abs of a * term
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
