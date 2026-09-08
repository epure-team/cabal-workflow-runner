#!/usr/bin/env bash
set -euo pipefail

invocation_dir=$(pwd -P)
root=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd -P)

resolve_executable() {
  local label=$1 candidate=$2 resolved dir base
  if [[ -z $candidate ]]; then
    echo "read-only backend selftest: $label is blank" >&2
    return 1
  fi
  if [[ $candidate == */* ]]; then
    resolved=$candidate
  elif ! resolved=$(type -P -- "$candidate"); then
    echo "read-only backend selftest: $label is not executable or on PATH" >&2
    return 1
  fi
  [[ $resolved == /* ]] || resolved=$invocation_dir/$resolved
  dir=$(dirname -- "$resolved")
  base=$(basename -- "$resolved")
  if ! dir=$(CDPATH='' cd -P -- "$dir" 2>/dev/null && pwd -P); then
    echo "read-only backend selftest: $label directory does not exist" >&2
    return 1
  fi
  resolved=$dir/$base
  if [[ ! -f $resolved || ! -x $resolved ]]; then
    echo "read-only backend selftest: $label is not an executable file" >&2
    return 1
  fi
  printf '%s\n' "$resolved"
}

cwr=$(resolve_executable CWR_BIN "${CWR_BIN:-$root/_build/default/bin/main.exe}")
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
mkdir -p "$tmp/bin" "$tmp/work" "$tmp/home/.cabal/adapters"

# Cabal probes and dispatches through this helper. Resolve it explicitly before
# restricting PATH so only the fixture's backend CLIs are discoverable.
if [[ -n ${CABAL_PROCESS_GROUP_LAUNCHER:-} ]]; then
  launcher_candidate=$CABAL_PROCESS_GROUP_LAUNCHER
else
  if ! opam_bin=$(opam var bin 2>/dev/null); then
    echo "read-only backend selftest: cannot locate the active opam bin directory" >&2
    exit 1
  fi
  launcher_candidate=$opam_bin/cabal-process-group-launcher
fi
launcher=$(resolve_executable CABAL_PROCESS_GROUP_LAUNCHER "$launcher_candidate")

cat > "$tmp/bin/claude" <<'SH'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$FAKE_CALLS"
if [[ ${1-} == --version ]]; then echo 'fake claude'; exit 0; fi
printf '%s\n' "$@" > "$FAKE_ARGV"
args=" $* "
if [[ $args != *' --disallowedTools '* ]] ||
   [[ $args != *'Bash'* ]] || [[ $args != *'Edit'* ]] ||
   [[ $args != *'Write'* ]] || [[ $args != *'WebSearch'* ]] ||
   [[ $args != *'WebFetch'* ]] || [[ $args == *' --allowedTools '* ]]; then
  : > "$FAKE_MUTATION"
fi
printf '{"type":"result","subtype":"success","is_error":false,"result":"{\\"ok\\":true}","structured_output":{"ok":true},"session_id":"70f62070-a552-4cc6-9ee2-b97cf02e3eda"}\n'
SH
cat > "$tmp/bin/codex" <<'SH'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$FAKE_CALLS"
if [[ ${1-} == --version ]]; then echo 'fake codex'; exit 0; fi
printf '%s\n' "$@" > "$FAKE_ARGV"
args=" $* "
if [[ $args != *' -s read-only '* ]] || [[ $args == *' --full-auto '* ]]; then
  : > "$FAKE_MUTATION"
fi
printf '{"type":"item.completed","item":{"type":"agent_message","text":"{\\"ok\\":true}"}}\n'
SH
chmod +x "$tmp/bin/claude" "$tmp/bin/codex"
cat > "$tmp/bin/unsafe-spoof" <<'SH'
#!/bin/bash
printf '%s %s\n' "${0##*/}" "$*" >> "$TRAP_CALLS"
: > "$FAKE_MUTATION"
printf '{"ok":true}\n'
SH
cp "$tmp/bin/unsafe-spoof" "$tmp/bin/opencode"
cp "$tmp/bin/unsafe-spoof" "$tmp/bin/not-registered"
chmod +x "$tmp/bin/unsafe-spoof" "$tmp/bin/opencode" "$tmp/bin/not-registered"
cat > "$tmp/home/.cabal/adapters/unknown-custom.yaml" <<'YAML'
name: unknown-custom
display_name: spoofed unsafe custom backend
invocation_command: unsafe-spoof
template_set: generic
timeout_seconds: 10
YAML

workflow() {
  local type=$1
  local field=''
  [[ $type == default ]] || field=",\"agent_type\":\"$type\""
  printf '{"name":"read-only-runtime","steps":[{"kind":"agent","id":"a","prompt":"p","read_only":true%s,"output_schema":{"ok":"bool"}}]}\n' "$field"
}

workspace_snapshot() {
  local path digest
  # These are the only Cabal-managed workspace files permitted to change.
  while IFS= read -r path; do
    digest=$(sha256sum -- "$path")
    printf '%s %s\n' "${digest%% *}" "${path#./}"
  done < <(
    find . -type f ! -path './.cabal/backend-config/*' \
      ! -path './.codex/config.toml' -print | sort
  )
}

run_safe() {
  local type=$1 expected=$2
  local backend=claude-code
  [[ $type == codex ]] && backend=codex
  workflow "$type" > "$tmp/work/workflow.json"
  rm -f "$tmp/argv" "$tmp/calls" "$tmp/trap-calls" "$tmp/mutated"
  local before
  before=$(cd "$tmp/work" && workspace_snapshot)
  if ! (cd "$tmp/work" && HOME="$tmp/home" PATH="$tmp/bin" \
      CABAL_PROCESS_GROUP_LAUNCHER="$launcher" \
      FAKE_ARGV="$tmp/argv" FAKE_CALLS="$tmp/calls" \
      TRAP_CALLS="$tmp/trap-calls" FAKE_MUTATION="$tmp/mutated" \
      CWR_BACKEND="$backend" "$cwr" run workflow.json > "$tmp/out" 2>&1); then
    cat "$tmp/out" >&2
    exit 1
  fi
  [[ ! -e $tmp/trap-calls && ! -e $tmp/mutated ]]
  [[ $(cd "$tmp/work" && workspace_snapshot) == "$before" ]]
  grep -Fqx -- '--version' "$tmp/calls"
  grep -qx -- "$expected" "$tmp/argv"
}

run_rejected() {
  local type=$1
  workflow "$type" > "$tmp/work/workflow.json"
  rm -f "$tmp/argv" "$tmp/calls" "$tmp/trap-calls" "$tmp/mutated"
  if (cd "$tmp/work" && HOME="$tmp/home" PATH="$tmp/bin" \
      CABAL_PROCESS_GROUP_LAUNCHER="$launcher" FAKE_ARGV="$tmp/argv" \
      FAKE_CALLS="$tmp/calls" TRAP_CALLS="$tmp/trap-calls" \
      FAKE_MUTATION="$tmp/mutated" \
      CWR_BACKEND=claude-code "$cwr" run workflow.json \
      > "$tmp/out" 2>&1); then
    echo "backend $type unexpectedly dispatched" >&2
    exit 1
  fi
  grep -Fq -- '"error":"agent_type does not match the bound live backend"' "$tmp/out"
  [[ ! -e $tmp/argv && ! -e $tmp/calls && ! -e $tmp/trap-calls ]]
  [[ ! -e $tmp/mutated ]]
}

# Central hardened dispatch may write its owned backend configuration (including
# Codex's project config), but the invoked backend must use the handwritten
# read-only argv contracts and leave every other workspace path unchanged.
run_safe claude-code Bash,Edit,Write,NotebookEdit,WebSearch,WebFetch
grep -qx -- '--disallowedTools' "$tmp/argv"
run_safe codex read-only
grep -qx -- '-s' "$tmp/argv"

# The explicit operator selection fixes the live backend. An omitted agent_type or
# one equal to that backend is safe; cross-backend, unknown, and YAML-spoofed
# request routing fails closed without dispatch.
run_safe default Bash,Edit,Write,NotebookEdit,WebSearch,WebFetch
run_rejected opencode
run_rejected not-registered
run_rejected unknown-custom

echo "read-only backend selftest: OK"
