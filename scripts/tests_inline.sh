#!/usr/bin/env bash
# ./tests_inline.sh src
#
# run all top level declarations matching "test*".
# They must have type IO Bool (success is True)
#
# example usage: 
#
# > ./tests_inline.sh hs
# vvvvvvvvvvvv Transform.Assets.test1 vvvvvvvvvvvv
# void main()
# {
#     s1 = _rl_LoadShader(s1, a, b);
#
#     Shader s2 = _rl_LoadShader(s2, a, b), s3 = _rl_LoadShader(s3, b, a);
#     Shader s4[2] = {_rl_LoadShader(s4, a, b), _rl_LoadShader(s4, c, d)};
# }
#
# ^^^^^^^^^^^^ Transform.Assets.test1 ^^^^^^^^^^^^
# vvvvvvvvvvvv Transform.Assets.test2 vvvvvvvvvvvv
# ^^^^^^^^^^^^ Transform.Assets.test2 ^^^^^^^^^^^^
# vvvvvvvvvvvv Transform.Assets.Macro.test5 vvvvvvvvvvvv
# ^^^^^^^^^^^^ Transform.Assets.Macro.test5 ^^^^^^^^^^^^
# successes: Transform.Assets.{test2,test1}
# rerun failures with:
# ghcid hs/Transform/Assets/Macro.hs -TTransform.Assets.Macro.test5
#
# it's much faster for ghcid to call ghci directly rather than cabal repl. But
# then extra flags (packages and extensions) have to be supplied for example:
#
# yq -r '."default-extensions"[]' package.yaml | sed 's/^/:set -X/g' > .ghci
# echo ":set -isrc" > .ghci
# chmod 755 . .ghci
#
# in cabal.project.local
# write-ghc-environment-files: always
set -euo pipefail

script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$(basename "$script_dir")" == "scripts" ]]; then
  cd "$script_dir/.."
else
  cd "$script_dir"
fi


srcdir=${1:-"hs"}
export raylibd_datadir=.

if [[ ! -d "dist-newstyle" ]]; then
  cabal build
fi

paths_raylibd=$(find dist-newstyle -name "Paths_raylibd.hs" | awk '{ print length, $0 }' | sort -n | head -n1 | cut -d" " -f2-)
if [[ -z "${paths_raylibd}" ]]; then
  echo "Paths_raylibd.hs not found under dist-newstyle" >&2
  exit 1
fi

clear
files=()
fns=()
qualtest=()
modules=()
while IFS= read -r file; do
  mod=$(echo "$file" | sed 's|'$srcdir'/||; s|\.hs$||; s|/|.|g')
  fnames=$(grep -Eo '^test[_a-zA-Z0-9'\'']*' "$file" | sort -u || true)
  [ -z "$fnames" ] || files+=($file)
  [ -z "$fnames" ] || modules+=($mod)
  for fname in $fnames; do
    fns+=$mod
    # TODO find the longest $mod.$fname and use that instead of 30
    # haskell printf keeps the extra chars so it's only aesthetic
    qualtest+=("do \
    let { bar n c = Text.Printf.printf \"\ESC[%dm%s %-30s %s\ESC[0m\n\" n (replicate 12 c) \"$mod.$fname\" (replicate 12 c) } ; \
          bar 33 'v' ; \
          b <- $mod.$fname ; \
          bar (if b then 32 else 31) '^'; \
          return $ if b then Left \"$mod.$fname\" else Right \"ghcid $file $paths_raylibd -T$mod.$fname\"")
  done
done < <(find hs/ -name "*.hs" | sort)

star_modules=()
for mod in "${modules[@]}"; do
  star_modules+=("*$mod")
done

ghci -v0 ${files[@]} -i$(dirname "$paths_raylibd") -ihs <<< ':set prompt ""
  :set prompt-cont ""
  :module +'${star_modules[@]}'
  (ok,notok) <- Data.Either.partitionEithers <$> sequence [ '$(IFS=,; echo "${qualtest[*]}")' ]
  :{
  data Trie = Trie Bool [(String, Trie)]

  splitDot :: String -> [String]
  splitDot s = case break (== '\''.'\'') s of
    (a, '\''.'\'':rest) -> a : splitDot rest
    (a, _) -> [a]

  emptyTrie :: Trie
  emptyTrie = Trie False []

  insertPath :: [String] -> Trie -> Trie
  insertPath [] (Trie _ ch) = Trie True ch
  insertPath (p:ps) (Trie term ch) = Trie term (go ch)
    where
      go [] = [(p, insertPath ps emptyTrie)]
      go ((k, t):rest)
        | k == p = (k, insertPath ps t) : rest
        | otherwise = (k, t) : go rest

  buildTrie :: [String] -> Trie
  buildTrie = foldr insertPath emptyTrie . map splitDot

  renderTrie :: Trie -> [String]
  renderTrie (Trie term ch) =
    let renderChild (k, t) =
          let subs = renderTrie t
              nonEmpty = filter (/= "") subs
              hasEmpty = any (== "") subs
              withSubs = case nonEmpty of
                [] -> []
                [s] -> [k ++ "." ++ s]
                _ -> [k ++ ".{" ++ Data.List.intercalate "," nonEmpty ++ "}"]
          in (if hasEmpty then [k] else []) ++ withSubs
    in (if term then [""] else []) ++ concatMap renderChild ch

  bashBrace :: [String] -> [String]
  bashBrace xs = filter (/= "") (renderTrie (buildTrie (Data.List.sort xs)))
  :}
  Control.Monad.unless (null ok) (putStrLn ("successes: " ++ unwords (bashBrace ok)))
  Control.Monad.unless (null notok) (putStrLn "rerun failures with:")
  mapM_ putStrLn notok'
