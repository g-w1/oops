module type Index = sig
  type t
  val of_int : int -> t
  val to_int : t -> int
  (* for debugging *)
  val pp : Format.formatter -> t -> unit
end
