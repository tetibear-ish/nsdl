#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
dune build
fail=0
for f in test/fixtures/*.nsdl; do
  if dune exec bin/main.exe -- "$f" > /dev/null; then
    echo "OK   $f"
  else
    echo "FAIL $f"
    fail=1
  fi
done
exit $fail
