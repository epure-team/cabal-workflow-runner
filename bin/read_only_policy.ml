(* Trusted CLI controls, never workflow/model-authored capability grants. *)
type t = { tools : string; budget_usd : float; max_turns : int }

let of_options tools budget_usd max_turns =
  match tools, budget_usd, max_turns with
  | None, None, None -> Ok None
  | Some tools, Some budget_usd, Some max_turns ->
      let names = String.split_on_char ',' tools |> List.map String.trim in
      if not (Float.is_finite budget_usd) || budget_usd <= 0. then
        Error "agent maximum budget must be finite and positive"
      else if max_turns <= 0 then Error "agent maximum turns must be positive"
      else if List.exists (fun name -> not (List.mem name ["Read";"Glob";"Grep"])) names
        || List.length names <> List.length (List.sort_uniq String.compare names) then
        Error "read-only tools must be a nonempty unique subset of Read,Glob,Grep"
      else Ok (Some {tools=String.concat "," names; budget_usd; max_turns})
  | _ -> Error "--read-only-tools, --agent-max-budget-usd and --agent-max-turns must be supplied together"

let apply policy (args, prompt) =
  (* Remove legacy bypass/deny flags: restricted mode refuses bypass, and an
     exact native toolset must not be confused with permission allowlists. *)
  let rec strip = function
    | "--dangerously-skip-permissions" :: rest -> strip rest
    | ("--disallowedTools" | "--allowedTools" | "--permission-mode" | "--setting-sources" | "--max-turns") :: _ :: rest -> strip rest
    | arg :: rest -> arg :: strip rest
    | [] -> [] in
  (strip args @ ["--safe-mode";"--restricted";"--permission-mode";"dontAsk";
    "--tools";policy.tools;"--strict-mcp-config";"--mcp-config";{|{"mcpServers":{}}|};
    "--max-budget-usd";Printf.sprintf "%.17g" policy.budget_usd;
    "--max-turns";string_of_int policy.max_turns], prompt)
