#!/usr/bin/env bash
set -euo pipefail

BIN=$1
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT

fail() {
  echo "approval ledger selftest: FAIL: $*" >&2
  exit 1
}

assert_eq() {
  local expected=$1 actual=$2 message=$3
  test "$actual" = "$expected" ||
    fail "$message (expected '$expected', got '$actual')"
}

assert_not_contains() {
  local needle=$1 path=$2 message=$3
  if grep -F "$needle" "$path" >/dev/null; then
    fail "$message"
  fi
}

# Harness mutation probes: prove the explicit assertion paths themselves reject
# inverted equality and forbidden-content conditions instead of relying on
# `set -e` behavior for `!` commands or non-final `&&` list elements.
if (assert_eq expected inverted assertion-probe) 2>/dev/null; then
  fail "assert_eq mutation probe false-passed"
fi
printf 'forbidden-probe\n' > "$tmp/assertion-probe"
if (assert_not_contains forbidden-probe "$tmp/assertion-probe" assertion-probe) 2>/dev/null; then
  fail "assert_not_contains mutation probe false-passed"
fi

cat > "$tmp/blocked.json" <<'JSON'
{"name":"approval-ledger-blocked","steps":[{"kind":"gate","id":"ready","when":{"lit":false}},{"kind":"commit","id":"publish"}]}
JSON
cat > "$tmp/committed.json" <<'JSON'
{"name":"approval-ledger-committed","steps":[{"kind":"gate","id":"ready","when":{"lit":true}},{"kind":"commit","id":"publish"}]}
JSON
cat > "$tmp/completed.json" <<'JSON'
{"name":"approval-ledger-completed","steps":[{"kind":"gate","id":"observed","when":{"lit":true}}]}
JSON
cat > "$tmp/aborted.json" <<'JSON'
{"name":"approval-ledger-aborted","steps":[{"kind":"run","id":"abort","cmd":["approval-ledger-abort"],"working_dir":".","stdout_schema":{"ok":"bool"}}]}
JSON
mkdir "$tmp/abort-bin"
printf '#!/bin/sh\nprintf "not-json\\n"\n' > "$tmp/abort-bin/approval-ledger-abort"
chmod +x "$tmp/abort-bin/approval-ledger-abort"

workflow_digest=$($BIN workflow-digest "$tmp/blocked.json")
token=approval-ledger-secret
token_digest="sha256:$(printf 'cwr.approval-token/v2\0%s' "$token" | sha256sum | cut -d' ' -f1)"
ctx='{"z":1}'
session=approval-ledger-session

rc=0
$BIN run --floor ready --approve "$token" --ledger "$tmp/blocked.ndjson" --ctx "$ctx" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null 2>&1 || rc=$?
assert_eq 2 "$rc" "blocked run exit code"
assert_eq ctx_snapshot "$(sed -n '1p' "$tmp/blocked.ndjson" | jq -r '.kind')" \
  "ledger first entry"
assert_eq 600 "$(stat -c %a "$tmp/blocked.ndjson")" "ledger private mode"
jq -e --arg token "$token_digest" --arg workflow "$workflow_digest" --arg session "$session" '
  .kind=="approval_supplied" and .token_digest==$token and .workflow_digest==$workflow and
  .session_nonce==$session and (.run_context_digest|test("^sha256:[0-9a-f]{64}$"))
' < <(sed -n '2p' "$tmp/blocked.ndjson") >/dev/null
assert_eq 1 "$(jq -c 'select(.kind=="approval_supplied")' "$tmp/blocked.ndjson" | wc -l)" \
  "singleton approval header"
assert_not_contains "$token" "$tmp/blocked.ndjson" "raw approval token leaked into ledger"

# An early block without runtime approval retains a ledger but cannot claim an
# approval-supplied identity.
rc=0
$BIN run --floor ready --ledger "$tmp/no-approval.ndjson" --ctx "$ctx" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null 2>&1 || rc=$?
assert_eq 2 "$rc" "approval-less blocked run exit code"
assert_eq "" "$(jq -c 'select(.kind=="approval_supplied")' "$tmp/no-approval.ndjson")" \
  "approval-less run emitted approval header"

# Run-start approval evidence and terminal trace persist for the remaining two
# engine outcome classes as well: Completed_no_commit and Aborted.
$BIN run --approve "$token" --ledger "$tmp/completed.ndjson" \
  "$tmp/completed.json" > "$tmp/completed.out" 2> "$tmp/completed.err" ||
  fail "Completed_no_commit run unexpectedly failed"
grep -F 'outcome: Completed_no_commit' "$tmp/completed.out" >/dev/null ||
  fail "completed run did not report Completed_no_commit"
assert_eq 1 "$(jq -c 'select(.kind=="approval_supplied")' "$tmp/completed.ndjson" | wc -l)" \
  "completed run approval header"
assert_eq gate_evaluated "$(tail -n 1 "$tmp/completed.ndjson" | jq -r '.kind')" \
  "completed run terminal trace"

rc=0
PATH="$tmp/abort-bin:$PATH" $BIN run --allow-run approval-ledger-abort \
  --approve "$token" --ledger "$tmp/aborted.ndjson" "$tmp/aborted.json" \
  > "$tmp/aborted.out" 2> "$tmp/aborted.err" || rc=$?
assert_eq 2 "$rc" "aborted run exit code"
grep -F 'outcome: Aborted(' "$tmp/aborted.out" >/dev/null ||
  fail "aborted run did not report Aborted"
assert_eq 1 "$(jq -c 'select(.kind=="approval_supplied")' "$tmp/aborted.ndjson" | wc -l)" \
  "aborted run approval header"
assert_eq 1 "$(jq -c 'select(.kind=="run_executed")' "$tmp/aborted.ndjson" | wc -l)" \
  "aborted run effect receipt"
assert_eq blocked_at "$(tail -n 1 "$tmp/aborted.ndjson" | jq -r '.kind')" \
  "aborted run terminal trace"

# Replay validates the actual workflow/session/context binding.
$BIN replay --floor ready --ledger "$tmp/blocked.ndjson" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null
{
  sed -n '1p' "$tmp/blocked.ndjson"
  sed -n '3,$p' "$tmp/blocked.ndjson"
} > "$tmp/legacy.ndjson"
$BIN replay --floor ready --ledger "$tmp/legacy.ndjson" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null
if $BIN replay --floor ready --ledger "$tmp/blocked.ndjson" \
  --attestation-session wrong-session "$tmp/blocked.json" >/dev/null 2>&1; then
  fail "approval ledger replay accepted wrong session"
fi

jq -c 'if .kind=="approval_supplied" then .workflow_digest=("sha256:"+("0"*64)) else . end' \
  "$tmp/blocked.ndjson" > "$tmp/tampered-workflow.ndjson"
if $BIN replay --floor ready --ledger "$tmp/tampered-workflow.ndjson" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null 2>&1; then
  fail "approval ledger replay accepted wrong workflow binding"
fi
jq -c 'if .kind=="ctx_snapshot" then .ctx.z=2 else . end' \
  "$tmp/blocked.ndjson" > "$tmp/tampered-context.ndjson"
if $BIN replay --floor ready --ledger "$tmp/tampered-context.ndjson" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null 2>&1; then
  fail "approval ledger replay accepted wrong initial context"
fi
jq -c 'if .kind=="ctx_snapshot" then .ctx.z=1.5 else . end' \
  "$tmp/blocked.ndjson" > "$tmp/float-context.ndjson"
rc=0
$BIN replay --floor ready --ledger "$tmp/float-context.ndjson" \
  --attestation-session "$session" "$tmp/blocked.json" > "$tmp/float-replay.out" 2> "$tmp/float-replay.err" || rc=$?
assert_eq 1 "$rc" "non-canonical replay exit code"
grep -F 'corrupt ledger:' "$tmp/float-replay.err" >/dev/null ||
  fail "non-canonical replay lacked controlled corrupt-ledger error"

# Invalid restricted-canonical run context is a controlled pre-execution error.
rc=0
$BIN run --floor ready --approve "$token" --ledger "$tmp/float-run.ndjson" \
  --ctx '{"x":1.5}' "$tmp/committed.json" > "$tmp/float-run.out" 2> "$tmp/float-run.err" || rc=$?
assert_eq 1 "$rc" "non-canonical run exit code"
grep -F -- '--ctx is non-canonical:' "$tmp/float-run.err" >/dev/null ||
  fail "non-canonical run lacked controlled input error"
{
  sed -n '1p' "$tmp/blocked.ndjson"
  sed -n '3p' "$tmp/blocked.ndjson"
  sed -n '2p' "$tmp/blocked.ndjson"
  sed -n '4,$p' "$tmp/blocked.ndjson"
} > "$tmp/misordered.ndjson"
if $BIN replay --floor ready --ledger "$tmp/misordered.ndjson" \
  --attestation-session "$session" "$tmp/blocked.json" >/dev/null 2>&1; then
  fail "approval ledger replay accepted a non-header approval event"
fi

# A committed run binds the same token digest in approval and Commit events.
$BIN run --floor ready --approve "$token" --ledger "$tmp/committed.ndjson" \
  --attestation-session "$session" "$tmp/committed.json" >/dev/null
jq -e --arg token "$token_digest" -s '
  (map(select(.kind=="approval_supplied"))[0].token_digest==$token) and
  (map(select(.kind=="committed_step"))[0].token_digest==$token)
' "$tmp/committed.ndjson" >/dev/null
jq -c 'if .kind=="approval_supplied" then .token_digest=("sha256:"+("0"*64)) else . end' \
  "$tmp/committed.ndjson" > "$tmp/tampered-token.ndjson"
if $BIN replay --floor ready --ledger "$tmp/tampered-token.ndjson" \
  --attestation-session "$session" "$tmp/committed.json" >/dev/null 2>&1; then
  fail "approval ledger replay accepted Commit token mismatch"
fi

# Ledger initialization is fail-closed and rejects unsafe aliases without
# truncating their targets or running workflow effects.
mkdir "$tmp/ledger-dir"
rc=0
$BIN run --floor ready --approve "$token" --ledger "$tmp/ledger-dir" \
  "$tmp/committed.json" > "$tmp/dir.out" 2> "$tmp/dir.err" || rc=$?
assert_eq 1 "$rc" "directory ledger rejection exit code"
assert_not_contains 'outcome: Committed' "$tmp/dir.out" \
  "directory ledger rejection reported Commit success"
printf 'symlink-target\n' > "$tmp/symlink-target"
ln -s "$tmp/symlink-target" "$tmp/symlink-ledger"
rc=0
$BIN run --floor ready --approve "$token" --ledger "$tmp/symlink-ledger" \
  "$tmp/committed.json" >/dev/null 2>&1 || rc=$?
assert_eq 1 "$rc" "symlink ledger rejection exit code"
assert_eq symlink-target "$(cat "$tmp/symlink-target")" \
  "symlink ledger clobbered its target"
printf 'hardlink-target\n' > "$tmp/hardlink-target"
ln "$tmp/hardlink-target" "$tmp/hardlink-ledger"
rc=0
$BIN run --floor ready --approve "$token" --ledger "$tmp/hardlink-ledger" \
  "$tmp/committed.json" >/dev/null 2>&1 || rc=$?
assert_eq 1 "$rc" "hard-link ledger rejection exit code"
assert_eq hardlink-target "$(cat "$tmp/hardlink-target")" \
  "hard-link ledger clobbered its target"

# Prefix and append failures are non-successful. Prefix failure occurs before
# the engine; append failure may follow effects but never reports Commit success.
rc=0
CWR_TEST_FAIL_LEDGER_PREFIX=1 $BIN run --floor ready --approve "$token" \
  --ledger "$tmp/prefix-fail.ndjson" "$tmp/committed.json" > "$tmp/prefix.out" 2> "$tmp/prefix.err" || rc=$?
assert_eq 1 "$rc" "injected prefix-write failure exit code"
assert_not_contains 'outcome: Committed' "$tmp/prefix.out" \
  "prefix-write failure reported Commit success"
rc=0
CWR_TEST_FAIL_LEDGER_APPEND=1 $BIN run --floor ready --approve "$token" \
  --ledger "$tmp/append-fail.ndjson" "$tmp/committed.json" > "$tmp/append.out" 2> "$tmp/append.err" || rc=$?
assert_eq 1 "$rc" "injected append-write failure exit code"
assert_not_contains 'outcome: Committed' "$tmp/append.out" \
  "append-write failure reported Commit success"
grep -F 'audit ledger incomplete after workflow effects' "$tmp/append.err" >/dev/null ||
  fail "append-write failure lacked incomplete-audit warning"
for injected in CWR_TEST_FAIL_LEDGER_PREFIX_FLUSH CWR_TEST_FAIL_LEDGER_APPEND_FLUSH CWR_TEST_FAIL_LEDGER_CLOSE; do
  rc=0
  env "$injected"=1 $BIN run --floor ready --approve "$token" \
    --ledger "$tmp/$injected.ndjson" "$tmp/committed.json" > "$tmp/$injected.out" 2> "$tmp/$injected.err" || rc=$?
  assert_eq 1 "$rc" "$injected exit code"
  assert_not_contains 'outcome: Committed' "$tmp/$injected.out" \
    "$injected reported Commit success"
done

# Replacing the pathname during an allowed Run cannot redirect the held fd and
# is detected before the CLI reports success.
mkdir "$tmp/bin"
cat > "$tmp/bin/replace-ledger" <<'SH'
#!/usr/bin/env bash
rm -f -- "$LEDGER_PATH"
mkdir -- "$LEDGER_PATH"
SH
chmod +x "$tmp/bin/replace-ledger"
cat > "$tmp/replace.json" <<'JSON'
{"name":"approval-ledger-replace","steps":[{"kind":"run","id":"replace","cmd":["replace-ledger"],"working_dir":"."},{"kind":"gate","id":"ready","when":{"eq":[{"path":"outputs.replace.exit"},{"lit":0}]}},{"kind":"commit","id":"publish"}]}
JSON
rc=0
PATH="$tmp/bin:$PATH" LEDGER_PATH="$tmp/replaced.ndjson" $BIN run --floor ready \
  --allow-run replace-ledger --approve "$token" --ledger "$tmp/replaced.ndjson" \
  "$tmp/replace.json" > "$tmp/replaced.out" 2> "$tmp/replaced.err" || rc=$?
assert_eq 1 "$rc" "ledger pathname replacement exit code"
test -d "$tmp/replaced.ndjson" || fail "pathname replacement fixture did not run"
assert_not_contains 'outcome: Committed' "$tmp/replaced.out" \
  "pathname replacement reported Commit success"

# A busy ledger is rejected before truncation.
printf 'busy-original\n' > "$tmp/busy.ndjson"
mkfifo "$tmp/release-lock"
(flock -x "$tmp/busy.ndjson" sh -c 'printf ready > "$1"; read _ < "$2"' sh \
  "$tmp/lock-ready" "$tmp/release-lock") & lock_pid=$!
until test -f "$tmp/lock-ready"; do :; done
rc=0
$BIN run --floor ready --approve "$token" --ledger "$tmp/busy.ndjson" \
  "$tmp/committed.json" >/dev/null 2>&1 || rc=$?
assert_eq 1 "$rc" "busy ledger rejection exit code"
assert_eq busy-original "$(cat "$tmp/busy.ndjson")" \
  "busy ledger was truncated"
printf 'release\n' > "$tmp/release-lock"
wait "$lock_pid"

# Explicit legacy committed fixture with the old 32-hex digest remains replayable.
jq -c 'select(.kind!="approval_supplied") | if .kind=="committed_step" then .token_digest="0123456789abcdef0123456789abcdef" else . end' \
  "$tmp/committed.ndjson" > "$tmp/legacy-committed.ndjson"
$BIN replay --floor ready --ledger "$tmp/legacy-committed.ndjson" \
  --attestation-session "$session" "$tmp/committed.json" >/dev/null

echo "approval ledger selftest: PASS"
