#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

cwr=${CWR_BIN:-"$root/_build/default/bin/main.exe"}
if [[ ! -x "$cwr" ]]; then
  opam exec -- dune build --root "$root" bin/main.exe
fi

tool="$tmp/pinned-tool"
workflow="$tmp/pinned.workflow.json"
ledger="$tmp/ledger.ndjson"

cat >"$tool" <<'TOOL'
#!/usr/bin/env bash
set -euo pipefail
origin="$CWR_PINNED_EXECUTABLE_ORIGIN_DIR/pinned-tool"
printf '%s\n' '#!/usr/bin/env bash' 'printf '\''{"executed_path":"replacement","marker":"wrong"}\n'\''' >"$origin"
chmod 700 "$origin"
printf '{"executed_path":"%s","marker":"verified"}\n' "$0"
TOOL
chmod 700 "$tool"
digest="sha256:$(sha256sum "$tool" | awk '{print $1}')"

cat >"$workflow" <<JSON
{
  "name": "pinned-run-selftest",
  "steps": [
    {
      "kind": "run",
      "id": "proof",
      "cmd": ["pinned-tool"],
      "working_dir": ".",
      "stdout_schema": {"executed_path": "string", "marker": "string"},
      "executable_digest": "$digest"
    }
  ]
}
JSON

expect_acquisition_failure() {
  local name=$1
  local candidate=$2
  local probe="$tmp/$name.workflow.json"
  cat >"$probe" <<JSON
{"name":"$name","steps":[{"kind":"run","id":"probe","cmd":["$name"],"working_dir":".","executable_digest":"$digest"}]}
JSON
  if (cd "$tmp" && PATH="$tmp:$PATH" "$cwr" run "$name.workflow.json" \
    --allow-run "$name" >/dev/null 2>&1); then
    echo "$candidate executable unexpectedly passed secure acquisition" >&2
    exit 1
  fi
}

ln "$tool" "$tmp/hardlinked-tool"
expect_acquisition_failure hardlinked-tool hardlinked
rm "$tmp/hardlinked-tool"
ln -s "$tool" "$tmp/symlinked-tool"
expect_acquisition_failure symlinked-tool symlinked
rm "$tmp/symlinked-tool"

(cd "$tmp" && PATH="$tmp:$PATH" "$cwr" run pinned.workflow.json \
  --allow-run pinned-tool --ledger ledger.ndjson >/dev/null)

python3 - "$ledger" "$tool" "$digest" <<'PY'
import json
import pathlib
import sys

ledger, origin, expected_digest = sys.argv[1:]
entries = [json.loads(line) for line in pathlib.Path(ledger).read_text().splitlines()]
run = next(entry for entry in entries if entry.get("kind") == "run_executed")
identity = run["executable"]
parsed = run["parsed"]
assert identity == {"path": origin, "digest": expected_digest}, identity
assert parsed["marker"] == "verified", parsed
assert parsed["executed_path"] != origin, parsed
assert "cwr-pinned-run-" in parsed["executed_path"], parsed
assert "replacement" in pathlib.Path(origin).read_text()
PY

if (cd "$tmp" && PATH="$tmp:$PATH" "$cwr" run pinned.workflow.json \
  --allow-run pinned-tool >/dev/null 2>&1); then
  echo "replacement executable unexpectedly passed the original digest pin" >&2
  exit 1
fi

printf 'pinned-run-selftest: ok\n'
