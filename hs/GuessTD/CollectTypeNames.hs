{-# LANGUAGE ApplicativeDo #-}

module GuessTD.CollectTypeNames where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Loc (SrcLoc(..), Loc(..), Pos, posCoff)
import Text.Regex.Applicative
import Text.Parser.Combinators
import Control.Lens hiding (enum)

import GuessTD.Lex
import GuessTD.Parsers
import Control.Monad

newtype M = M { unM :: Map String SrcLoc }
instance Semigroup M where
  M a <> M b = M (M.unionWith keepEarliest  a b)
instance Monoid M where
 mempty = M mempty

type P = RE Tok M

typePrefix :: P
typePrefix = many (qualifierTok <|> symbolTok "*") *> typeCore

typeCore :: P
typeCore = structUnion
  <|> enum
  <|> (mempty <$ builtinTok)
  <|> (singletonName <$> msymG _TIdent)

structUnion :: P
structUnion = do
  keywordTok "struct" <|> keywordTok "union"
  tag <- omsymG _TIdent
  inner <- omsymG _TBlock
  pure $ let
      tagNames = namesFromList tag
      innerNames = mconcat (collectTypeNames . splitStatements . snd <$> inner)
    in (tagNames <> innerNames)

enum :: P
enum = do
  keywordTok "enum"
  tag <- omsymG _TIdent
  omsymG _TBlock
  pure (namesFromList tag)

param :: P
param = do
  names <- typePrefix
  many tokNotComma
  pure names

params :: P
params = mconcat <$> (param `sepBy` symbolTok ",")

function :: P
function = do
  ret <- typePrefix
  void (msymG _TIdent)
  params <- parenParams
  many tokNotComma
  pure (ret <> params)

parenParams :: P
parenParams = fromMaybe mempty . match params . snd <$> msymG _TParen

typedef :: P
typedef = do
  keywordTok "typedef"
  names <- typePrefix
  aliases <- typedefAliases
  many tokNotComma
  pure (names <> namesFromList aliases)

decl :: P
decl = do
  names <- typePrefix
  declarator `sepBy1` symbolTok ","
  pure names

collectNested :: [Tok] -> M
collectNested toks = mconcat [ collectTypeNames (splitStatements inner) | TBlock _ inner <- toks ]

parenGroups :: [Tok] -> [[Tok]]
parenGroups = go []
  where
    go acc [] = reverse acc
    go acc (TParen _ inner:ts) =
      let acc' = inner : parenGroups inner ++ acc
        in go acc' ts
    go acc (_:ts) = go acc ts

collectParenParams :: [Tok] -> M
collectParenParams toks =
  let declTokens = takeWhile (not . isAssign) toks
      matchParams group =
        case group of
          (TSymbol _ "*":_) -> mempty
          _ -> fromMaybe mempty (match params group)
    in mconcat (matchParams <$> parenGroups declTokens)
  where
    isAssign t = case t of
      TSymbol _ "=" -> True
      _ -> False

collectAll :: [Tok] -> M
collectAll toks =
  case match (typedef <|> function <|> decl) toks of
    Just names -> names <> collectParenParams toks <> collectNested toks
    Nothing -> collectNested toks

collectTypeNames :: [[Tok]] -> M
collectTypeNames statements = mconcat (map collectAll statements)

namesFromList :: [NameLoc] -> M
namesFromList = M . foldl insertEarliest mempty

singletonName :: NameLoc -> M
singletonName = namesFromList . pure

insertEarliest ::  Map String SrcLoc -> NameLoc -> Map String SrcLoc
insertEarliest m (loc, name) = M.insertWith keepEarliest name loc m

keepEarliest :: SrcLoc -> SrcLoc -> SrcLoc
keepEarliest newLoc oldLoc
  | srcLocOffset newLoc < srcLocOffset oldLoc = newLoc
  | otherwise = oldLoc

srcLocOffset :: SrcLoc -> Int
srcLocOffset (SrcLoc loc) = case loc of
  NoLoc -> maxBound
  Loc start _ -> posCoff start
