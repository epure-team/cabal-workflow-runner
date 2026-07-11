val read_regular : string -> (string, string) result
val write_atomic_noreplace : root:string -> relative:string -> content:string ->
  ([ `Written | `Published_uncertain ], string) result
