open! Base

type t = { ht : (int, unit) Hashtbl.t } [@@deriving sexp_of]

let create () = { ht = Hashtbl.create (module Int) }

let add set elem =
  ignore (Hashtbl.add set.ht ~key:elem ~data:());
  ()
;;

let mem set elem = Hashtbl.mem set.ht elem
let remove set elem = Hashtbl.remove set.ht elem
