(** A pure, offline, yojson-only linter over workflows, designed to be embedded
    by a meta-agent that builds its own workflows dynamically.

    [Lint] is the {b single source of truth} for the safety floor: error-severity
    diagnostics are {e exactly} the parse/shape failures plus the floor checks
    that {!Validate.workflow} enforces. The contract is:

    {b a workflow with no error-severity diagnostics is guaranteed to validate}

    ({!Validate.workflow} is itself defined in terms of {!check}, so the gate and
    the linter cannot drift). Warnings are legal + runnable but likely-mistaken
    shapes a generator might emit (e.g. a dangling output reference); they never
    fail the floor.

    Everything here is pure, instant, and backend-free — free to call in a tight
    generate -> lint -> fix loop. {!check_json} is {b parse-tolerant}: malformed
    JSON or shape errors become diagnostics and it {b never raises}. *)

type severity =
  | Error  (** makes the workflow unrunnable (floor / parse / shape failure). *)
  | Warning  (** legal + runnable, but a generator likely erred. *)

type diagnostic = {
  severity : severity;
  code : string;
      (** STABLE machine code an embedder can branch on, e.g.
          ["ungoverned-loop"]. See the table in [SPEC.md]. *)
  message : string;  (** agent- and human-readable explanation. *)
  loc : string;
      (** JSON path to the offending node, e.g. ["steps[3].body[0]"] or
          ["steps[2].governors"]; ["$"] for whole-document errors. *)
}

val diagnostic_to_json : diagnostic -> Yojson.Safe.t
(** [{"severity":"error"|"warning","code":..,"message":..,"loc":..}]. *)

val to_json : diagnostic list -> Yojson.Safe.t
(** [{"diagnostics":[ .. ]}] — the wire form an embedder feeds back to a
    generator agent. *)

val has_errors : diagnostic list -> bool
(** [true] iff any diagnostic has severity {!Error}. A workflow with no error
    diagnostics is guaranteed to {!Validate.workflow}. *)

val check : ?floor_gates:string list -> Types.workflow -> diagnostic list
(** Post-parse: ALL floor + semantic checks over a typed workflow, collected in
    one pass (never first-error-only). [floor_gates] defaults to [[]]. *)

val check_json : ?floor_gates:string list -> string -> diagnostic list
(** THE meta-agent entry point: lint a generator's RAW string output.
    Parse-tolerant — malformed JSON becomes a single [invalid-json] error and a
    shape error becomes [invalid-shape]; this function {b never raises}. On a
    well-formed workflow it returns exactly {!check}'s diagnostics. *)
