(* Total predicate DSL over a run context of recorded JSON outputs.

   Totality is the load-bearing property: [eval] ALWAYS returns a bool and never
   raises and never diverges. A missing path, a type mismatch, or a comparison of
   incomparable values all yield a defined result (the surrounding predicate is
   [false]). There are no recursion / iteration constructs in the DSL, so a single
   [eval] is structurally bounded by the size of the expression. *)

type value =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of value list

type t =
  | Path of string list
  | Lit of value
  | Eq of t * t
  | Ne of t * t
  | Lt of t * t
  | Le of t * t
  | Gt of t * t
  | Ge of t * t
  | In of t * t
  | And of t list
  | Or of t list
  | Not of t
  | Exists of string list

(* ---- value <-> json ---------------------------------------------------- *)

let rec value_of_json (j : Yojson.Safe.t) : value =
  match j with
  | `Null -> Null
  | `Bool b -> Bool b
  | `Int n -> Int n
  | `Intlit s -> ( match int_of_string_opt s with Some n -> Int n | None -> String s)
  | `Float f -> Float f
  | `String s -> String s
  | `List l -> List (List.map value_of_json l)
  | `Assoc _ -> Null (* objects are not comparable scalars; treat as Null *)

(* ---- path resolution (total) ------------------------------------------- *)

(* Resolve a dotted path over [ctx]. The first segment is a step id whose value
   is looked up in [ctx]; subsequent segments index into JSON objects. Anything
   that does not resolve (missing key, indexing a non-object) yields [None]. *)
let resolve ~(ctx : (string * Yojson.Safe.t) list) (path : string list) :
    value option =
  match path with
  | [] -> None
  | root :: rest -> (
      match List.assoc_opt root ctx with
      | None -> None
      | Some j0 ->
          let rec descend (j : Yojson.Safe.t) segs =
            match segs with
            | [] -> Some (value_of_json j)
            | key :: more -> (
                match j with
                | `Assoc fields -> (
                    match List.assoc_opt key fields with
                    | Some j' -> descend j' more
                    | None -> None)
                | _ -> None)
          in
          descend j0 rest)

(* Resolve a dotted path to the RAW JSON at that location (the same descent as
   [resolve], but the terminal case returns the JSON itself rather than the
   reduced scalar). Used by [Exists] so that a present object/array — which
   [value_of_json] would flatten to [Null] — is still seen as present. Total:
   any unresolved path yields [None]. *)
let resolve_raw ~(ctx : (string * Yojson.Safe.t) list) (path : string list) :
    Yojson.Safe.t option =
  match path with
  | [] -> None
  | root :: rest -> (
      match List.assoc_opt root ctx with
      | None -> None
      | Some j0 ->
          let rec descend (j : Yojson.Safe.t) segs =
            match segs with
            | [] -> Some j
            | key :: more -> (
                match j with
                | `Assoc fields -> (
                    match List.assoc_opt key fields with
                    | Some j' -> descend j' more
                    | None -> None)
                | _ -> None)
          in
          descend j0 rest)

(* ---- evaluation to a value (total) ------------------------------------- *)

(* Reduce an expression to a [value option]: [None] means "no defined value"
   (missing path). Boolean combinators do not reduce to a value; they are handled
   only at the [eval] (bool) level, so here we map them to [None]. *)
let rec to_value ~ctx (e : t) : value option =
  match e with
  | Path p -> resolve ~ctx p
  | Lit v -> Some v
  | _ -> None

(* ---- total comparison --------------------------------------------------- *)

(* Numeric coercion: Int/Float compare numerically; everything else only
   compares within its own kind. Returns [None] when the two values are not
   comparable (so the predicate becomes [false]). *)
and compare_values (a : value) (b : value) : int option =
  match (a, b) with
  | Int x, Int y -> Some (compare x y)
  | Float x, Float y -> Some (compare x y)
  | Int x, Float y -> Some (compare (float_of_int x) y)
  | Float x, Int y -> Some (compare x (float_of_int y))
  | String x, String y -> Some (compare x y)
  | Bool x, Bool y -> Some (compare x y)
  | Null, Null -> Some 0
  | List xs, List ys -> compare_lists xs ys
  | _ -> None

and compare_lists xs ys =
  match (xs, ys) with
  | [], [] -> Some 0
  | [], _ -> Some (-1)
  | _, [] -> Some 1
  | x :: xt, y :: yt -> (
      match compare_values x y with
      | Some 0 -> compare_lists xt yt
      | other -> other)

and value_eq (a : value) (b : value) : bool =
  match compare_values a b with Some 0 -> true | _ -> false

(* ---- total boolean evaluation ------------------------------------------ *)

and eval ~(ctx : (string * Yojson.Safe.t) list) (e : t) : bool =
  match e with
  | Lit (Bool b) -> b
  | Lit _ -> false
  | Path p -> ( match resolve ~ctx p with Some (Bool b) -> b | _ -> false)
  (* [Exists] uses RAW-JSON presence, not the reduced scalar: a present
     object/array/scalar => true; an explicit JSON [null] or a missing path =>
     false. (Comparison operators keep using the value-based [resolve].) *)
  | Exists p -> ( match resolve_raw ~ctx p with None | Some `Null -> false | Some _ -> true)
  | Not e -> not (eval ~ctx e)
  | And es -> List.for_all (eval ~ctx) es
  | Or es -> List.exists (eval ~ctx) es
  | Eq (a, b) -> binop ~ctx a b (fun x y -> value_eq x y)
  | Ne (a, b) -> binop ~ctx a b (fun x y -> not (value_eq x y))
  | Lt (a, b) -> cmp ~ctx a b (fun c -> c < 0)
  | Le (a, b) -> cmp ~ctx a b (fun c -> c <= 0)
  | Gt (a, b) -> cmp ~ctx a b (fun c -> c > 0)
  | Ge (a, b) -> cmp ~ctx a b (fun c -> c >= 0)
  | In (a, b) -> (
      match (to_value ~ctx a, to_value ~ctx b) with
      | Some x, Some (List ys) -> List.exists (fun y -> value_eq x y) ys
      | _ -> false)

(* Apply a binary value predicate; absent operands => false. *)
and binop ~ctx a b f =
  match (to_value ~ctx a, to_value ~ctx b) with
  | Some x, Some y -> f x y
  | _ -> false

(* Ordered comparison; incomparable or absent => false. *)
and cmp ~ctx a b f =
  match (to_value ~ctx a, to_value ~ctx b) with
  | Some x, Some y -> ( match compare_values x y with Some c -> f c | None -> false)
  | _ -> false

(* ---- value <-> string (for diagnostics / json round-trip of literals) -- *)

let rec json_of_value (v : value) : Yojson.Safe.t =
  match v with
  | Null -> `Null
  | Bool b -> `Bool b
  | Int n -> `Int n
  | Float f -> `Float f
  | String s -> `String s
  | List l -> `List (List.map json_of_value l)
