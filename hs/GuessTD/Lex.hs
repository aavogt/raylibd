{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module GuessTD.Lex where

import Data.Char
import Data.List
import Data.Loc (Loc(..), Pos (..), advancePos, startPos)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.Applicative
import Control.Lens
import PyF
import Text.Show.Pretty

newtype SLoc = SLoc Loc deriving Eq

instance Show SLoc where
  showsPrec _ (SLoc (Loc (Pos f line col char) (Pos f' line' col' char')))
    | f    /= f'    = ('"':) . (f++) . ('-':) . (f'++) . (':':) . shows line  . ('-':) . shows line'  . ('"':)
    | line /= line' = ('"':) . (f++) . (':':) . shows line  . ('-':) . shows line'  . ('"':)
    | col  /= col'  = ('"':) . (f++) . (':':) . shows line  . (':':) . shows col   . ('-':) . shows col'  . ('"':)
    | char /= char' = ('"':) . (f++) . (':':) . shows line  . (':':) . shows col   . (':':) . shows char . ('-':) . shows char' . ('"':)
    | otherwise     = ('"':) . (f++) . (':':) . shows line  . (':':) . shows col   . (':':) . shows char . ('"':)

data Tok
  = TIdent SLoc String
  | TNumber SLoc String
  | TKeyword SLoc String
  | TSymbol SLoc String
  | TParen SLoc [Tok]
  | TBlock SLoc [Tok]
  deriving (Eq, Show)

makePrisms ''Tok

keywords :: Set String
keywords = Set.fromList
  [ "typedef", "struct", "union", "enum"
  , "const", "static", "extern", "register", "auto", "restrict"
  , "void", "char", "short", "int", "long", "float", "double"
  , "signed", "unsigned", "true", "false"
  , "return", "if", "else", "for", "while", "switch", "case"
  , "break", "continue", "do", "goto", "sizeof"
  ]

builtinTypes :: Set String
builtinTypes = Set.fromList
  [ "void", "char", "short", "int", "long", "float", "double"
  , "signed", "unsigned"
  ]

qualifiers :: Set String
qualifiers = Set.fromList
  [ "const", "static", "extern", "register", "auto", "restrict" ]

symbolChars :: Set Char
symbolChars = Set.fromList ";:,(){}[]*=.-+<>!/|&^%"

isKeyword :: String -> Bool
isKeyword = (`Set.member` keywords)

isQualifier :: String -> Bool
isQualifier = (`Set.member` qualifiers)

isBuiltin :: String -> Bool
isBuiltin = (`Set.member` builtinTypes)

lineMarker = do
  string "# "
  line <- some (psym isDigit)
  sym ' '
  sym '"'
  file <- some (psym (/= '"'))
  sym '"'
  pure (Pos file (read line) 1 0 )


testPos :: IO Bool
testPos = do
  let tokens = lexTokens [fmt|
# 40 "/usr/lib/gcc/x86_64-linux-gnu/15/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 103 "/usr/lib/gcc/x86_64-linux-gnu/15/include/stdarg.h" 3 4
typedef __gnuc_va_list va_list;
# 89 "/usr/local/include/raylib.h" 2 3
|]
  pPrint tokens
  return (not $ "noLoc" `isInfixOf` show tokens)

lexTokens :: String -> [Tok]
lexTokens = go (startPos "<input>")
  where
    go _ [] = []
    go _ (findLongestPrefix lineMarker -> Just (newPos, rest)) = go newPos (dropWhile (/= '\n') rest)
    go pos input@(stripPrefix "//" -> Just cs) =
      let pos' = advancePos (advancePos pos '/') '/'
          (pos'', rest) = skipLine pos' cs
       in go pos'' rest
    go pos (c:cs)
      | c == '#' =
          let pos' = advancePos pos '#'
              (pos'', rest) = skipLine pos' cs
           in go pos'' rest
      | isSpace c = go (advancePos pos c) cs
      | c == '"' =
          let (pos', rest) = skipString (advancePos pos '"') cs
           in go pos' rest
      | c == '\'' =
          let (pos', rest) = skipChar (advancePos pos '\'') cs
           in go pos' rest
      | otherwise =
          case findLongestPrefix tokRE (c:cs) of
            Just ((kind, lexeme), rest) ->
              let pos' = advancePosBy pos lexeme
                  loc = Loc pos pos'
                  tok = mkTok kind (SLoc loc) lexeme
               in tok : go pos' rest
            Nothing -> go (advancePos pos c) cs

    tokRE :: RE Char (TokKind, String)
    tokRE = identRE <|> numberRE <|> symbolRE

    identRE =
      let first = psym (\ch -> isAlpha ch || ch == '_')
          rest = many (psym (\ch -> isAlphaNum ch || ch == '_'))
       in fmap (\name -> (TokIdent, name)) ((:) <$> first <*> rest)

    -- does 123.123, -123e5 etc. matter?
    -- maybe Tok should just be TTok | TBlock
    numberRE = (TokNumber,) <$> some (psym isDigit)

    symbolRE =
      let symTok ch = (TokSymbol, [ch])
       in foldr1 (<|>) (map (fmap symTok . sym) (Set.toList symbolChars))

    mkTok kind loc name = case kind of
      TokIdent
        | isKeyword name -> TKeyword loc name
        | otherwise -> TIdent loc name
      TokNumber -> TNumber loc name
      TokSymbol -> TSymbol loc name

    advancePosBy = foldl' advancePos

    skipLine pos (stripPrefix "\\\n" -> Just xs) = skipLine (advancePosBy pos "\\\n") xs
    skipLine pos ('\n':xs) = (advancePos pos '\n', xs)
    skipLine pos (x:xs) = skipLine (advancePos pos x) xs
    skipLine pos [] = (pos, [])

    skipString = goEsc
      where
        goEsc pos [] = (pos, [])
        goEsc pos (x:xs)
          | x == '\\' = case xs of
              [] -> (advancePos pos x, [])
              (y:rest) -> goEsc (advancePos (advancePos pos x) y) rest
          | x == '"' = (advancePos pos x, xs)
          | otherwise = goEsc (advancePos pos x) xs

    skipChar = goEsc
      where
        goEsc pos [] = (pos, [])
        goEsc pos (x:xs)
          | x == '\\' = case xs of
              [] -> (advancePos pos x, [])
              (y:rest) -> goEsc (advancePos (advancePos pos x) y) rest
          | x == '\'' = (advancePos pos x, xs)
          | otherwise = goEsc (advancePos pos x) xs

data TokKind = TokIdent | TokNumber | TokSymbol deriving Show

collapseBlocks :: [Tok] -> [Tok]
collapseBlocks = fst . go
  where
    go [] = ([], [])
    go (TSymbol loc "{":ts) =
      let (inner, rest) = consume 1 [] ts
          collapsed = collapseBlocks inner
          (after, leftover) = go rest
       in (TBlock loc collapsed : after, leftover)
    go (t:ts) =
      let (after, rest) = go ts
       in (t : after, rest)

    consume _ acc [] = (reverse acc, [])
    consume depth acc (TSymbol loc "{":ts) = consume (depth + 1) (TSymbol loc "{" : acc) ts
    consume depth acc (TSymbol loc "}":ts)
      | depth == 1 = (reverse acc, ts)
      | otherwise = consume (depth - 1) (TSymbol loc "}" : acc) ts
    consume depth acc (t:ts) = consume depth (t : acc) ts

collapseParens :: [Tok] -> [Tok]
collapseParens = fst . go
  where
    go [] = ([], [])
    go (TBlock loc inner:ts) =
      let collapsedInner = collapseParens inner
          (after, rest) = go ts
       in (TBlock loc collapsedInner : after, rest)
    go (TSymbol loc "(":ts) =
      let (inner, rest) = consume 1 [] ts
          collapsed = collapseParens inner
          (after, leftover) = go rest
       in (TParen loc collapsed : after, leftover)
    go (t:ts) =
      let (after, rest) = go ts
       in (t : after, rest)

    consume _ acc [] = (reverse acc, [])
    consume depth acc (TSymbol loc "(":ts) = consume (depth + 1) (TSymbol loc "(" : acc) ts
    consume depth acc (TSymbol loc ")":ts)
      | depth == 1 = (reverse acc, ts)
      | otherwise = consume (depth - 1) (TSymbol loc ")" : acc) ts
    consume depth acc (t:ts) = consume depth (t : acc) ts

splitStatements :: [Tok] -> [[Tok]]
splitStatements = go []
  where
    go current [] = [reverse current | not (null current)]
    go current (TSymbol _ ";" : ts) = reverse current : go [] ts
    go current (t@(TBlock _ _) : ts)
      | isFunctionLike current = reverse (t : current) : go [] ts
      | otherwise = go (t : current) ts
    go current (t:ts) = go (t : current) ts

    isFunctionLike toks =
      let hasParen = any isParen toks
          hasTypedef = any isTypedef toks
       in hasParen && not hasTypedef

    isParen t = case t of
      TParen {} -> True
      _ -> False

    isTypedef t = case t of
      TKeyword _ "typedef" -> True
      _ -> False
