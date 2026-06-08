(** A small, {b total} predicate DSL evaluated over a run context.

    The run context is an association list mapping a step id to that step's
    recorded structured JSON output. Paths address into it as
    [["outputs"; "<id>"; "<field>"; ...]] — by convention the first segment is
    a binding key (the engine binds an agent's output under
    ["outputs.<id>"] and the loop index under ["loop"]).

    {b Totality is mandatory.} {!val:eval} ALWAYS returns a [bool]: it never
    raises and never diverges. A missing path, a type mismatch, or comparing
    incomparable types all yield a defined result (the surrounding predicate is
    [false]). There are no recursion / iteration constructs, so a single
    evaluation is bounded by the size of the expression. This is what lets a
    governed loop's stop decision always terminate even when the run itself may
    be open-ended. *)

type value =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of value list

type t =
  | Path of string list  (** e.g. [["outputs"; "assess"; "severity"]]. *)
  | Lit of value
  | Eq of t * t
  | Ne of t * t
  | Lt of t * t
  | Le of t * t
  | Gt of t * t
  | Ge of t * t
  | In of t * t  (** [In (x, xs)]: [x] is a member of the list [xs]. *)
  | And of t list
  | Or of t list
  | Not of t
  | Exists of string list  (** path resolves to a present, non-null value. *)

val eval : ctx:(string * Yojson.Safe.t) list -> t -> bool
(** Evaluate a predicate over [ctx]. TOTAL: always returns a bool, never raises,
    never diverges. *)

val value_of_json : Yojson.Safe.t -> value
(** Project a JSON value into a DSL [value] (objects/variants become [Null]). *)

val json_of_value : value -> Yojson.Safe.t
(** Inverse for scalar/list values (used to round-trip literals through JSON). *)
