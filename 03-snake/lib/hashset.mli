open! Base

type t [@@deriving sexp_of]

(** [create] initializes a Set representation that is backed by a hash table. *)
val create : unit -> t

(** [add] the element to the set *)
val add : t -> int -> unit

(** [mem] gives true if the element is in the set *)
val mem : t -> int -> bool

(** [remove] the element from the set *)
val remove : t -> int -> unit
