{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module GuessTD.Lex where

import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.Applicative
import Control.Lens

data Tok
  = TIdent String
  | TNumber String
  | TKeyword String
  | TSymbol String
  | TParen [Tok]
  | TBlock [Tok]
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

lexTokens :: String -> [Tok]
lexTokens = go
  where
    go [] = []
    go (stripPrefix "//" -> Just cs) = go (dropWhile (/= '\n') cs)
    go (c:cs)
      | c == '#' = go (skipLine cs)
      | isSpace c = go cs
      | c == '"' = go (skipString cs)
      | c == '\'' = go (skipChar cs)
      | otherwise =
          case findLongestPrefix tokRE (c:cs) of
            Just (tok, rest) -> tok : go rest
            Nothing -> go cs

    tokRE :: RE Char Tok
    tokRE = identRE <|> numberRE <|> symbolRE

    identRE =
      let first = psym (\ch -> isAlpha ch || ch == '_')
          rest = many (psym (\ch -> isAlphaNum ch || ch == '_'))
       in fmap mkIdent ((:) <$> first <*> rest)

    -- does 123.123, -123e5 etc. matter?
    -- maybe Tok should just be TTok | TBlock
    numberRE = TNumber <$> some (psym isDigit)

    symbolRE =
      let symTok ch = TSymbol [ch]
       in foldr1 (<|>) (map (fmap symTok . sym) (Set.toList symbolChars))

    mkIdent name
      | isKeyword name = TKeyword name
      | otherwise = TIdent name

    skipLine (stripPrefix "\\\n" -> Just xs) = skipLine xs
    skipLine ('\n':xs) = xs
    skipLine (x:xs) = skipLine xs
    skipLine xs = xs

    skipString = goEsc
      where
        goEsc [] = []
        goEsc (x:xs)
          | x == '\\' = case xs of
              [] -> []
              (_:rest) -> goEsc rest
          | x == '"' = xs
          | otherwise = goEsc xs

    skipChar = goEsc
      where
        goEsc [] = []
        goEsc (x:xs)
          | x == '\\' = case xs of
              [] -> []
              (_:rest) -> goEsc rest
          | x == '\'' = xs
          | otherwise = goEsc xs

collapseBlocks :: [Tok] -> [Tok]
collapseBlocks = fst . go
  where
    go [] = ([], [])
    go (TSymbol "{":ts) =
      let (inner, rest) = consume 1 [] ts
          collapsed = collapseBlocks inner
          (after, leftover) = go rest
       in (TBlock collapsed : after, leftover)
    go (t:ts) =
      let (after, rest) = go ts
       in (t : after, rest)

    consume _ acc [] = (reverse acc, [])
    consume depth acc (TSymbol "{":ts) = consume (depth + 1) (TSymbol "{" : acc) ts
    consume depth acc (TSymbol "}":ts)
      | depth == 1 = (reverse acc, ts)
      | otherwise = consume (depth - 1) (TSymbol "}" : acc) ts
    consume depth acc (t:ts) = consume depth (t : acc) ts

collapseParens :: [Tok] -> [Tok]
collapseParens = fst . go
  where
    go [] = ([], [])
    go (TBlock inner:ts) =
      let collapsedInner = collapseParens inner
          (after, rest) = go ts
       in (TBlock collapsedInner : after, rest)
    go (TSymbol "(":ts) =
      let (inner, rest) = consume 1 [] ts
          collapsed = collapseParens inner
          (after, leftover) = go rest
       in (TParen collapsed : after, leftover)
    go (t:ts) =
      let (after, rest) = go ts
       in (t : after, rest)

    consume _ acc [] = (reverse acc, [])
    consume depth acc (TSymbol "(":ts) = consume (depth + 1) (TSymbol "(" : acc) ts
    consume depth acc (TSymbol ")":ts)
      | depth == 1 = (reverse acc, ts)
      | otherwise = consume (depth - 1) (TSymbol ")" : acc) ts
    consume depth acc (t:ts) = consume depth (t : acc) ts

splitStatements :: [Tok] -> [[Tok]]
splitStatements = go []
  where
    go current [] = [reverse current | not (null current)]
    go current (TSymbol ";" : ts) = reverse current : go [] ts
    go current (t@(TBlock _) : ts)
      | isFunctionLike current = reverse (t : current) : go [] ts
      | otherwise = go (t : current) ts
    go current (t:ts) = go (t : current) ts

    isFunctionLike toks =
      let hasParen = any isParen toks
          hasTypedef = TKeyword "typedef" `elem` toks
       in hasParen && not hasTypedef

    isParen t = case t of
      TParen _ -> True
      _ -> False
