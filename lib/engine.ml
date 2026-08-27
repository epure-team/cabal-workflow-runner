(* Thin re-export shell: the actual interpreter lives in {!Engine_state}
   (execution-state and small stateless helpers), {!Engine_dynamic_parallel}
   (Dynamic_parallel [over] resolution and template instantiation),
   {!Engine_run} ([run]) and {!Engine_replay} ([replay]). See engine.mli for
   the documented public interface. *)

let token_digest = Engine_state.token_digest
let run = Engine_run.run

exception Replay_mismatch = Engine_replay.Replay_mismatch

let replay = Engine_replay.replay
