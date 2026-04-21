#!/bin/bash

# Exit immediately if any command fails
set -e
# Use yq to extract the default-extensions list
extensions=$(yq -r '."default-extensions"[]' package.yaml)

# Handle empty list or invalid YAML
if [ -z "$extensions" ]; then
  echo "Warning: No extensions found in package.yaml."
  exit 0
fi

temp_pragmas=$(mktemp)
# Generate the pragma lines
pragma_lines=""
for ext in $extensions; do
  # Ensure each extension is properly quoted and formatted
  echo "{-# LANGUAGE $ext #-}"
done > $temp_pragmas

for file in "$@"; do
  if [ ! -f "$file" ]; then
    echo "Error: File '$file' not found."
    continue
  fi

  # Prepend the pragmas to the file
  cat "$temp_pragmas" "$file" > "$file.tmp"
  mv "$file.tmp" "$file"
done

# Clean up temporary file
rm "$temp_pragmas"
echo "Inserted GHC pragmas into all specified files."
