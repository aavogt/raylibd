{-# LANGUAGE ApplicativeDo #-}

module GuessTD.Parsers where

import Data.Functor
import Text.Regex.Applicative
import Data.Maybe
import Control.Lens
import Data.Monoid (First)
import Text.Parser.Combinators

import GuessTD.Lex

-- | 'msym' except takes a prism/traversal
--
-- > msymG :: Traversal' s a -> RE s a
-- > msymG :: Lens' s a -> RE s a
msymG :: Getting (First a) s a -> RE s a
msymG f = msym (^? f)

omsymG :: Getting (First a) s a -> RE s [a]
omsymG f = maybeToList <$> optional (msymG f)

keywordTok :: String -> RE Tok String
keywordTok kw = msymG $ _TKeyword . filtered (==kw)

symbolTok :: String -> RE Tok String
symbolTok symb = msymG $ _TSymbol . filtered (==symb)

qualifierTok :: RE Tok String
qualifierTok = msymG $ _TKeyword . filtered isQualifier

builtinTok :: RE Tok String
builtinTok = msymG $ _TKeyword . filtered isBuiltin

tokNotComma :: RE Tok Tok
tokNotComma = msymG $ filtered (allOf _TSymbol (/= ","))

tokNotCommaNotIdent :: RE Tok Tok
tokNotCommaNotIdent = psym \case
  TSymbol "," -> False
  TIdent _ -> False
  _ -> True

tokNotCommaNotIdentOrParenIdent :: RE Tok Tok
tokNotCommaNotIdentOrParenIdent = psym \case
  TSymbol "," -> False
  TIdent _ -> False
  TParen inner -> isNothing (firstIdentInParen inner)
  _ -> True

typedefAliases :: RE Tok [String]
typedefAliases = catMaybes <$> decl `sepBy1` symbolTok ","
  where
    decl = do
      many tokNotCommaNotIdentOrParenIdent
      name <- identOrParenIdent
      many tokNotComma
      pure name

    identOrParenIdent =
      pure <$> msymG _TIdent <|> firstIdentInParen <$> msymG _TParen

firstIdentInParen :: [Tok] -> Maybe String
firstIdentInParen = listToMaybe . go
  where
    go [] = []
    go (TIdent name:_) = [name]
    go (TParen inner:rest) = go inner ++ go rest
    go (_:rest) = go rest

declarator :: RE Tok ()
declarator = do
  many (symbolTok "*")
  void (msymG _TIdent) <|> parenDeclarator
  many tokNotCommaNotIdent
  pure ()
  where
    parenDeclarator = void (msym matchParen)

    matchParen = \case
      TParen inner
        | parenHasPointerIdent inner -> Just inner
      _ -> Nothing

parenHasPointerIdent :: [Tok] -> Bool
parenHasPointerIdent = go False
  where
    go sawPointer [] = sawPointer && isJust (firstIdentInParen [])
    go sawPointer (TSymbol "*":rest) = go True rest
    go sawPointer (TParen inner:rest) = go sawPointer inner || go sawPointer rest
    go sawPointer (TIdent _:_) = sawPointer
    go sawPointer (_:rest) = go sawPointer rest
