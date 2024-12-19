open Symbols

module type PERM = functor (A : ATOM) ->
sig
  type t

  type supp = A.t list
  val id : t
  val comp : t -> t -> t
  val inv : t -> t
  val get_supp : t -> supp
  val get_diff : t -> t -> A.t list
  val to_string : t -> string
end

(* module type FPermutations = PERM with  *)

module type FP = functor (A : ATOM) -> sig type t end with type t = A.t

module FuncPerm = functor (A : ATOM) -> struct
  type t = A.t -> A.t

  type supp = A.t list

  let id = Fun.id

  let comp = Fun.compose

  let inv p = failwith "testing"

  let  get_supp p = failwith "testing"

  let get_diff p = failwith " testing"

  let  to_string p = failwith "testing"
end
