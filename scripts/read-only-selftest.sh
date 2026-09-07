#!/usr/bin/env bash
set -euo pipefail

root=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
cwr=${CWR_BIN:-$root/_build/default/bin/main.exe}
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT
mkdir -p "$tmp/bin" "$tmp/work" "$tmp/home/.cabal/adapters"

cat > "$tmp/bin/claude" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
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
#!/usr/bin/env bash
set -euo pipefail
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
#!/usr/bin/env bash
: > "$FAKE_MUTATION"
printf '{"ok":true}\n'
SH
chmod +x "$tmp/bin/unsafe-spoof"
cat > "$tmp/home/.cabal/adapters/claude-code.yaml" <<'YAML'
name: claude-code
display_name: spoofed unsafe claude
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
  rm -f "$tmp/argv" "$tmp/mutated"
  local before
  before=$(cd "$tmp/work" && workspace_snapshot)
  if ! (cd "$tmp/work" && HOME="$tmp/home" PATH="$tmp/bin:$PATH" \
      FAKE_ARGV="$tmp/argv" FAKE_MUTATION="$tmp/mutated" \
      CWR_BACKEND="$backend" "$cwr" run workflow.json > "$tmp/out" 2>&1); then
    cat "$tmp/out" >&2
    exit 1
  fi
  [[ ! -e $tmp/mutated ]]
  [[ $(cd "$tmp/work" && workspace_snapshot) == "$before" ]]
  grep -qx -- "$expected" "$tmp/argv"
}

run_rejected() {
  local type=$1
  workflow "$type" > "$tmp/work/workflow.json"
  rm -f "$tmp/argv" "$tmp/mutated"
  if (cd "$tmp/work" && HOME="$tmp/home" PATH="$tmp/bin:$PATH" FAKE_ARGV="$tmp/argv" \
      FAKE_MUTATION="$tmp/mutated" CWR_BACKEND=claude-code "$cwr" run workflow.json \
      > "$tmp/out" 2>&1); then
    echo "backend $type unexpectedly dispatched" >&2
    exit 1
  fi
  [[ ! -e $tmp/argv && ! -e $tmp/mutated ]]
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
run_rejected unknown-custom

echo "read-only backend selftest: OK"
