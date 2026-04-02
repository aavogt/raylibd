-- | what would happen if typedefs were inferred as best they can?
-- that is, we always seem to have a parse error
--
-- f (T x) { 
-- }
-- which can be fixed by adding T to the typedefs list
--
-- Does Transform.hs even depend on correct disambiguation of T * a; f(a);
module ParseTD where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Char
import Data.List
import Data.Loc
import Data.Typeable
import Language.C
import qualified Data.ByteString.Char8 as C8
import Text.PrettyPrint.Mainland hiding ((<|>))
import Text.Show.Pretty
import qualified Data.Sequence as Seq
import Control.Applicative
import Data.Sequence (Seq)
import Debug.Trace

-- | replacement for `Language.C.Parser.parse` which 
-- checks for parse error on `Token` and then reruns the parser with it as a typename
parseAddingTD exts typnames p bs pos = parseAddingTD1 [] bslines exts typnames p bs pos
  where bslines = Seq.fromList (C8.lines bs)

parseAddingTD1 ss bslines exts typnames p bs pos = case parse exts typnames p bs pos of
  Left (SomeException (cast >=> docToId bslines -> Just s))
    | traceShow s $ not (null s), s `notElem` typnames, isIdent s
      -> parseAddingTD1 (s:ss) bslines exts (s : typnames) p bs pos
  a -> (ss, a) -- and plumb loc?

docToId :: Seq C8.ByteString -> ParserException -> Maybe String
docToId bslines (ParserException (Loc _ (Pos _ line col char)) doc) = fromLoc <|> fromMsg where
  -- typedef right of the error position
  fromMsg = do
    candidate <- stripPrefix "parse error on `" (pretty 1000 doc) <&> takeWhile (/='\'')
    guard (isIdent candidate)
    Just candidate
  -- typedef left of the error position
  fromLoc = do
    guard $ "parse error on `{'" `isPrefixOf` pretty 1000 doc
    line <- bslines ^? ix (line-1)
    -- TODO do C8.unpack later / not at all so docToId :: _ -> Maybe ByteString?
    Just $ reverse $ takeWhile isAlphaNum $ dropWhile (not . isAlphaNum) $ reverse $ C8.unpack $ C8.take col line

-- ghcid ParseTD -TtdFail
-- 
-- this case should use fromLoc even though `x' is in the error message.
-- don't use Alternative Maybe <|>
-- but I don't want to backtrack 2^n parses 
tdFail = case parseAddingTD [] [] parseUnit (C8.pack "void f(Vector x){ return; };") (Just (Pos "tdFail.c" 1 1 0)) of
  (added, Left (SomeException e)) -> pPrint (added, e) -- 1:15: parse error on `x'
  x -> pPrint x

tdOk2 = case parseAddingTD [] [] parseUnit (C8.pack "auto x = (Vector){ 1, 2 };") (Just (Pos "tdFail.c" 1 1 0)) of
  (added, Left (SomeException e)) -> pPrint (added, e)
  x -> pPrint x

-- ghcid ParseTD -TtdOk
tdOk = case parseAddingTD [] [] parseUnit (C8.pack "Vector x = (Vector){ 1, 2 };") (Just (Pos "tdOk.c" 1 1 0)) of
  (added, Left (SomeException e)) -> pPrint (added, e)
  x -> pPrint x

isIdent :: String -> Bool
isIdent (x:xs) = isAlpha x && all isAlphaNum xs
isIdent _ = False
