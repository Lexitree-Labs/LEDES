(** Parser validator for LEDES1998B format *)

type input = string
(** type of input *)

type err = string
(** type of error *)

val parse_and_validate_greedy : input list -> err list
(** [parse_and_validate_greedy n] takes [n] as lines from a LEDES1998B file and
    collect all parsing and validation error(s). This is the slow path. *)

val parse_and_validate_eager : string list -> unit
(** [parse_and_validate_eager n] takes [n] as lines from a LEDES1998B file and
    eagerly bubbles up the first error. This is the fast path *)
