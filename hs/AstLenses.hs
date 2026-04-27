-- generic-lens would be nice if Language.C.Quote derived generic
module AstLenses where

import Control.Lens
import Language.C.Quote
import Data.Loc

funcParams :: Lens' Func Params
funcParams op (Func a b c d e f) = op d <&> \b' -> Func a b c d e f
funcParams op (OldFunc a b c [] e f g) = op (Params [] False noLoc) <&> \(Params ps _ _) -> OldFunc a b c (getId ps) e f g

getId :: [Param] -> [Id]
getId = map (\(Param (Just a) _ _ _) -> a)

funcName :: Lens' Func String
funcName op (Func a (Id b c) d e f g) = op b <&> \b' -> Func a (Id b' c) d e f g
funcName op (OldFunc a (Id b c) d e f g h) = op b <&> \b' -> OldFunc a (Id b' c) d e f g h

funcPtr :: Lens' Func Decl
funcPtr op (Func a b c d e f) = op c <&> \c -> Func a b c d e f
funcPtr op (OldFunc a b c d e f g) = op c <&> \c -> OldFunc a b c d e f g

funcDs :: Lens' Func DeclSpec
funcDs op (Func a b c d e f) = op a <&> \a' -> Func a' b c d e f
funcDs op (OldFunc a b c d e f g) = op a <&> \a' -> OldFunc a' b c d e f g

funcId :: Lens' Func Id
funcId op (Func a b c d e f) = op b <&> \b' -> Func a b' c d e f
funcId op (OldFunc a b c d e f g) = op b <&> \b' -> OldFunc a b' c d e f g

-- could be Data.Data.Lens.template?
funcBlockItems :: Lens' Func [BlockItem]
funcBlockItems op (Func a b c d bs f) = op bs <&> \bs' -> Func a b c d bs' f
funcBlockItems op (OldFunc a b c d e bs f) = op bs <&> \bs' -> OldFunc a b c d e bs' f
