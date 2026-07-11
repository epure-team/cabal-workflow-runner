val validate : Yojson.Safe.t -> (unit, string) result
val validate_no_duplicates : Yojson.Safe.t -> (unit, string) result
val to_string : Yojson.Safe.t -> (string, string) result
