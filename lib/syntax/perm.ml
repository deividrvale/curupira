open Symbols

module type PERM =
sig
  type p

  type a

  type supp = a list
  val id : p
  val comp : p -> p -> p
  val inv : p -> p
  val get_supp : p -> supp
  val get_diff : p -> p -> a list
  val to_string : p -> string
end

(* module type FPermutations = PERM with  *)

module type FP = functor (A : ATOM) -> PERM with type a = A.t and type p = A.t -> A.t

module FuncPerm : FP = functor (A : ATOM) -> struct
  type p = A.t -> A.t
  type a = A.t

  type supp = a list

  let id = Fun.id

  let comp = Fun.compose

  let inv p = failwith "testing"

  let  get_supp p = failwith "testing"

  let get_diff p = failwith " testing"

  let  to_string p = failwith "testing"
end
