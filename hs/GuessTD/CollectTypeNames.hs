{-# LANGUAGE ApplicativeDo #-}

module GuessTD.CollectTypeNames where

import Data.Set (Set)
import qualified Data.Set as S
import Text.Regex.Applicative
import Data.Maybe
import Text.Parser.Combinators
import Control.Lens hiding (enum)

import GuessTD.Lex
import GuessTD.Parsers

type P = RE Tok (Set String)

typePrefix :: P
typePrefix = many (qualifierTok <|> symbolTok "*") *> typeCore

typeCore :: P
typeCore = structUnion
  <|> enum
  <|> (Empty <$ builtinTok)
  <|> (S.singleton <$> msymG _TIdent)

structUnion :: P
structUnion = do
  keywordTok "struct" <|> keywordTok "union"
  tag <- omsymG _TIdent
  inner <- omsymG _TBlock
  pure (S.fromList tag <> S.unions (collectTypeNames . splitStatements <$> inner))

enum :: P
enum = do
  keywordTok "enum"
  tag <- omsymG _TIdent
  omsymG _TBlock
  pure (S.fromList tag)

param :: P
param = do
  names <- typePrefix
  many tokNotComma
  pure names

params :: P
params = S.unions <$> (param `sepBy` symbolTok ",")

function :: P
function = do
  ret <- typePrefix
  msymG _TIdent
  params <- parenParams
  many tokNotComma
  pure (ret <> params)

parenParams :: P
parenParams = fromMaybe mempty . match params <$> msymG _TParen

typedef :: P
typedef = do
  keywordTok "typedef"
  names <- typePrefix
  aliases <- typedefAliases
  many tokNotComma
  pure (names <> S.fromList aliases)

decl :: P
decl = do
  names <- typePrefix
  declarator `sepBy1` symbolTok ","
  pure names

collectNested :: [Tok] -> Set String
collectNested toks = S.unions [ collectTypeNames (splitStatements inner) | TBlock inner <- toks ]

parenGroups :: [Tok] -> [[Tok]]
parenGroups = go []
  where
    go acc [] = reverse acc
    go acc (TParen inner:ts) =
      let acc' = inner : parenGroups inner ++ acc
        in go acc' ts
    go acc (_:ts) = go acc ts

collectParenParams :: [Tok] -> Set String
collectParenParams toks =
  let declTokens = takeWhile (not . isAssign) toks
      matchParams group =
        case group of
          (TSymbol "*":_) -> mempty
          _ -> fromMaybe mempty (match params group)
    in S.unions (matchParams <$> parenGroups declTokens)
  where
    isAssign t = case t of
      TSymbol "=" -> True
      _ -> False

collectAll :: [Tok] -> Set String
collectAll toks =
  case match (typedef <|> function <|> decl) toks of
    Just names -> names <> collectParenParams toks <> collectNested toks
    Nothing -> collectNested toks

collectTypeNames :: [[Tok]] -> Set String
collectTypeNames statements = S.unions (map collectAll statements)
