{-# LANGUAGE ApplicativeDo #-}

module GuessTD.Parsers where

import Data.Functor
import Text.Regex.Applicative
import Data.Maybe
import Control.Lens
import Data.Monoid (First)
import Text.Parser.Combinators

import GuessTD.Lex
import Data.Loc
import Data.Tuple

type NameLoc = (SLoc, String)

type ParenLoc = (SLoc, [Tok])

type BlockLoc = (SLoc, [Tok])

-- | 'msym' except takes a prism/traversal
--
-- > msymG :: Traversal' s a -> RE s a
-- > msymG :: Lens' s a -> RE s a
msymG :: Getting (First a) s a -> RE s a
msymG f = msym (^? f)

omsymG :: Getting (First a) s a -> RE s [a]
omsymG f = maybeToList <$> optional (msymG f)

identTok :: RE Tok NameLoc
identTok = msymG _TIdent

parenTok :: RE Tok ParenLoc
parenTok = msymG _TParen

blockTok :: RE Tok BlockLoc
blockTok = msymG _TBlock

keywordTok :: String -> RE Tok String
keywordTok kw = snd <$> msymG (_TKeyword . filtered ((==kw) . snd))

symbolTok :: String -> RE Tok String
symbolTok symb = snd <$> msymG (_TSymbol . filtered ((==symb) . snd))

qualifierTok :: RE Tok String
qualifierTok = snd <$> msymG (_TKeyword . filtered (isQualifier . snd))

builtinTok :: RE Tok String
builtinTok = snd <$> msymG (_TKeyword . filtered (isBuiltin . snd))

tokNotComma :: RE Tok Tok
tokNotComma = msymG $ filtered (allOf _TSymbol ((/= ",") . snd))

tokNotCommaNotIdent :: RE Tok Tok
tokNotCommaNotIdent = psym \case
  TSymbol _ "," -> False
  TIdent _ _ -> False
  _ -> True

tokNotCommaNotIdentOrParenIdent :: RE Tok Tok
tokNotCommaNotIdentOrParenIdent = psym \case
  TSymbol _ "," -> False
  TIdent _ _ -> False
  TParen _ inner -> isNothing (firstIdentInParen inner)
  _ -> True

typedefAliases :: RE Tok [NameLoc]
typedefAliases = catMaybes <$> decl `sepBy1` symbolTok ","
  where
    decl = do
      many tokNotCommaNotIdentOrParenIdent
      name <- identOrParenIdent
      many tokNotComma
      pure name

    identOrParenIdent =
      pure <$> identTok <|> firstIdentInParen . snd <$> parenTok

firstIdentInParen :: [Tok] -> Maybe NameLoc
firstIdentInParen = listToMaybe . go
  where
    go [] = []
    go (TIdent loc name:_) = [(loc, name)]
    go (TParen _ inner:rest) = go inner ++ go rest
    go (_:rest) = go rest

declarator :: RE Tok ()
declarator = do
  many (symbolTok "*")
  void identTok <|> parenDeclarator
  many tokNotCommaNotIdent
  pure ()
  where
    parenDeclarator = void (msym matchParen)

    matchParen = \case
      TParen _ inner
        | parenHasPointerIdent inner -> Just inner
      _ -> Nothing

parenHasPointerIdent :: [Tok] -> Bool
parenHasPointerIdent = go False
  where
    go sawPointer [] = sawPointer && isJust (firstIdentInParen [])
    go sawPointer (TSymbol _ "*":rest) = go True rest
    go sawPointer (TParen _ inner:rest) = go sawPointer inner || go sawPointer rest
    go sawPointer (TIdent _ _:_) = sawPointer
    go sawPointer (_:rest) = go sawPointer rest
