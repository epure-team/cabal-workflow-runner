#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

cwr=${CWR_BIN:-"$root/_build/default/bin/main.exe"}
if [[ ! -x "$cwr" ]]; then
  opam exec --switch=/home/mathias/dev/cabal -- dune build --root "$root" bin/main.exe
fi

workflow="$tmp/attest.workflow.json"
seed="$tmp/seed"
public="$tmp/public.json"
other_public="$tmp/other-public.json"
ledger="$tmp/ledger.ndjson"

printf '%032d' 0 >"$seed"
cat >"$workflow" <<'JSON'
{
  "name": "attestation-cli-selftest",
  "version": "1.0",
  "steps": [
    {
      "kind": "attest",
      "id": "export",
      "select": ["campaign"],
      "replay_domain": "cwr-selftest/v1",
      "output": "result.attestation.json"
    }
  ]
}
JSON
digest=$("$cwr" workflow-digest "$workflow")
node "$root/scripts/verify-attestation.mjs" --self-test
(cd "$tmp" && "$cwr" lint attest.workflow.json --json >/dev/null)
(cd "$tmp" && "$cwr" lint ./attest.workflow.json --json >/dev/null)

exec 3<"$seed"
"$cwr" attestation-public-key --attestation-key-fd 3 >"$public"

exec 3<"$seed"
"$cwr" run "$workflow" --ctx '{"campaign":{"id":"C-1"}}' \
  --attestation-key-fd 3 --attestation-root "$tmp" \
  --attestation-session session-001 --ledger "$ledger" \
  --require-attestation export --expected-workflow-digest "$digest"

test -s "$tmp/result.attestation.json"
test -s "$ledger"
node "$root/scripts/verify-attestation.mjs" "$tmp/result.attestation.json" \
  --public-identity "$public" --workflow-digest "$digest" --step export \
  --domain cwr-selftest/v1 --session session-001 --occurrence 0 \
  --output-path result.attestation.json \
  --selected-json '{"campaign":{"id":"C-1"}}'
"$cwr" verify-attestation "$workflow" \
  --attestation "$tmp/result.attestation.json" --step export \
  --attestation-public-key "$public" --attestation-session session-001 \
  --expected-workflow-digest "$digest" --ctx '{"campaign":{"id":"C-1"}}'
"$cwr" replay "$workflow" --ledger "$ledger" \
  --attestation-public-key "$public" --attestation-session session-001 \
  --require-attestation export --expected-workflow-digest "$digest"
(cd "$tmp" && "$cwr" replay attest.workflow.json --ledger ledger.ndjson \
  --attestation-public-key public.json --attestation-session session-001 \
  --require-attestation export --expected-workflow-digest "$digest")
(cd "$tmp" && "$cwr" verify-attestation attest.workflow.json \
  --attestation result.attestation.json --step export \
  --attestation-public-key ./public.json --attestation-session session-001 \
  --expected-workflow-digest "$digest" --ctx '{"campaign":{"id":"C-1"}}')

mkdir "$tmp/dot-root"
exec 3<"$seed"
(cd "$tmp/dot-root" && "$cwr" run "$workflow" \
  --ctx '{"campaign":{"id":"C-1"}}' --attestation-key-fd 3 \
  --attestation-root . --attestation-session session-dot \
  --require-attestation export --expected-workflow-digest "$digest")
test -s "$tmp/dot-root/result.attestation.json"

expect_verify_failure() {
  local log="$tmp/verify-failure.log"
  if "$cwr" verify-attestation "$@" >"$log" 2>&1; then
    echo "invalid durable attestation unexpectedly verified" >&2
    exit 1
  fi
  if grep -Eq 'Fatal error|Invalid_argument|Raised at' "$log"; then
    echo "invalid durable attestation escaped as an uncaught exception" >&2
    exit 1
  fi
}

expect_unsafe_verify_failure() {
  local selected=$1
  local log="$tmp/unsafe-verify-failure.log"
  if "$cwr" verify-attestation "${common[@]}" --ctx "$selected" \
    >"$log" 2>&1; then
    echo "unsafe selected value unexpectedly verified" >&2
    exit 1
  fi
  grep -q 'INVALID ATTESTATION' "$log"
  if grep -Eq 'Fatal error|Invalid_argument|Raised at' "$log"; then
    echo "unsafe selected value escaped as an uncaught exception" >&2
    exit 1
  fi
}

common=("$workflow" --attestation "$tmp/result.attestation.json" --step export \
  --attestation-public-key "$public" --attestation-session session-001 \
  --expected-workflow-digest "$digest")
expect_verify_failure "${common[@]}" --ctx '{"campaign":{"id":"wrong"}}'
expect_unsafe_verify_failure '{"campaign":9007199254740992}'
expect_unsafe_verify_failure '{"campaign":1.5}'
expect_unsafe_verify_failure '{"campaign":999999999999999999999999999}'
expect_verify_failure "$workflow" --attestation "$tmp/result.attestation.json" \
  --step export --attestation-public-key "$public" \
  --attestation-session wrong-session --expected-workflow-digest "$digest" \
  --ctx '{"campaign":{"id":"C-1"}}'
expect_verify_failure "$workflow" --attestation "$tmp/result.attestation.json" \
  --step wrong --attestation-public-key "$public" \
  --attestation-session session-001 --expected-workflow-digest "$digest" \
  --ctx '{"campaign":{"id":"C-1"}}'

sed 's/attestation-cli-selftest/other-workflow/' "$workflow" >"$tmp/wrong.workflow.json"
expect_verify_failure "$tmp/wrong.workflow.json" \
  --attestation "$tmp/result.attestation.json" --step export \
  --attestation-public-key "$public" --attestation-session session-001 \
  --expected-workflow-digest "$digest" --ctx '{"campaign":{"id":"C-1"}}'

printf '%032d' 1 >"$tmp/other-seed"
exec 3<"$tmp/other-seed"
"$cwr" attestation-public-key --attestation-key-fd 3 >"$other_public"
expect_verify_failure "$workflow" --attestation "$tmp/result.attestation.json" \
  --step export --attestation-public-key "$other_public" \
  --attestation-session session-001 --expected-workflow-digest "$digest" \
  --ctx '{"campaign":{"id":"C-1"}}'

mkdir "$tmp/attacker"
exec 3<"$tmp/other-seed"
"$cwr" run "$workflow" --ctx '{"campaign":{"id":"C-1"}}' \
  --attestation-key-fd 3 --attestation-root "$tmp/attacker" \
  --attestation-session session-001 --require-attestation export \
  --expected-workflow-digest "$digest"
if node "$root/scripts/verify-attestation.mjs" \
  "$tmp/attacker/result.attestation.json" --public-identity "$public" \
  --workflow-digest "$digest" --step export --domain cwr-selftest/v1 \
  --session session-001 --occurrence 0 --output-path result.attestation.json \
  --selected-json '{"campaign":{"id":"C-1"}}'; then
  echo "attacker self-signed artifact passed independent pin" >&2
  exit 1
fi

printf '{}\n' >"$tmp/bad.attestation.json"
expect_verify_failure "$workflow" --attestation "$tmp/bad.attestation.json" \
  --step export --attestation-public-key "$public" \
  --attestation-session session-001 --expected-workflow-digest "$digest" \
  --ctx '{"campaign":{"id":"C-1"}}'

if "$cwr" replay "$workflow" --ledger "$ledger" \
  --attestation-public-key "$public" --attestation-session wrong-session \
  --require-attestation export --expected-workflow-digest "$digest"; then
  echo "cross-session replay unexpectedly succeeded" >&2
  exit 1
fi

if "$cwr" run "$workflow" --ctx '{"campaign":{"id":"C-1"}}' \
  --attestation-root "$tmp" --attestation-session session-002 \
  --require-attestation export --expected-workflow-digest "$digest"; then
  echo "missing-key attest unexpectedly succeeded" >&2
  exit 1
fi

expect_unsafe_run_failure() {
  local label=$1
  local selected=$2
  local unsafe_root="$tmp/unsafe-$label"
  local log="$tmp/unsafe-$label.log"
  mkdir "$unsafe_root"
  exec 3<"$seed"
  if "$cwr" run "$workflow" --ctx "$selected" \
    --attestation-key-fd 3 --attestation-root "$unsafe_root" \
    --attestation-session "unsafe-$label" --require-attestation export \
    --expected-workflow-digest "$digest" >"$log" 2>&1; then
    echo "unsafe $label selection unexpectedly produced an attestation" >&2
    exit 1
  fi
  test ! -e "$unsafe_root/result.attestation.json"
  grep -qi 'blocked' "$log"
  if grep -Eq 'Fatal error|Invalid_argument|Raised at' "$log"; then
    echo "unsafe $label selection escaped as an uncaught exception" >&2
    exit 1
  fi
}

expect_unsafe_run_failure int '{"campaign":9007199254740992}'
expect_unsafe_run_failure float '{"campaign":1.5}'
expect_unsafe_run_failure intlit \
  '{"campaign":999999999999999999999999999}'

printf '{"name":"empty","steps":[]}\n' >"$tmp/empty.workflow.json"
printf '' >"$tmp/empty.ledger.ndjson"
if "$cwr" replay "$tmp/empty.workflow.json" \
  --ledger "$tmp/empty.ledger.ndjson" --attestation-public-key "$public" \
  --attestation-session session-001 --require-attestation export \
  --expected-workflow-digest "$digest"; then
  echo "empty forged workflow/ledger bypassed required attestation" >&2
  exit 1
fi

echo "attestation CLI selftest: OK"
