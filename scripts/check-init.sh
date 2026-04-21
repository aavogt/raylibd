#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$(basename "$script_dir")" == "scripts" ]]; then
  cd "$script_dir/.."
else
  cd "$script_dir"
fi

shopt -s nullglob

files=(init/[0-9]*-*.c)

if (( ${#files[@]} == 0 )); then
  echo "No matching files in init/." >&2
  exit 0
fi

dup=$(printf '%s\n' "${files[@]}" \
  | sed -E 's#^init/([0-9]+)-.*\.c$#\1#' \
  | sort \
  | uniq -d)

if [[ -n "$dup" ]]; then
  echo "Duplicate numeric prefixes found:" >&2
  printf '  %s\n' $dup >&2
  exit 1
fi

echo "OK: numeric prefixes are unique."
