#!/usr/bin/env bash
# Usage: ./gen-tags.sh package1 package2 ...
# Requires: hothasktags, ghc-pkg or cabal

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$(basename "$script_dir")" == "scripts" ]]; then
  cd "$script_dir/.."
else
  cd "$script_dir"
fi

# ./tags.sh base cmdargs fsnotify lens containers time directory filepath bytestring process language-c-quote mainland-pretty srcloc pretty-show
set -e

PACKAGES=("$@")
SRCDIR=deps

output=`pwd`/tags

cd $SRCDIR
for pkg in "${PACKAGES[@]}"; do
  # Get source dirs registered with ghc-pkg
  src=$(ghc-pkg field "$pkg" hs-libraries 2>/dev/null || true)
  cabal unpack "$pkg" 2>/dev/null || echo "Warning: couldn't unpack $pkg"
done
cd ../

find $SRCDIR -name '*.hs' | xargs hothasktags >> $output
sort -o $output $output

echo "Done. Tags written to ./tags"
