{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Transform.Common
  ( StateSpec (..),
    StateField (..),
    UseRewrite (..),
    pattern Fun,
    uniqueDummy,
    applyRewrites,
    buildStateMembers,
    genLoops,
    genLoopsST,
    indexExpr,
    fieldFromDecl,
    constToArrayLen,
    initToExpr,
    initExprAt,
    positionalInitAt,
    declArraySize,
    declTypeSpec,
    declrHasntFun,
    isStaticSpec,
    identName,
    mkIdent,
    mkMember,
    mkMemberFrom,
    mkMemberIndexFrom,
  )
where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Control.Monad.Trans.State
import Data.Data (Data)
import Data.List
import Data.Loc (noLoc)
import qualified Data.Set as S
import Language.C hiding (mkIdent)
import Language.C.Quote.C

pattern Fun :: String -> [BlockItem] -> Func
pattern Fun id block <-
  ( \case
      Func _ id _ _ block _ -> (identName id, block)
      OldFunc _ id _ _ _ block _ -> (identName id, block) ->
      (id, block)
    )

data StateSpec = StateSpec
  { fields :: [StateField],
    initStmts :: [Stm],
    useRewrite :: Id -> [UseRewrite],
    hoistedNames :: [String]
  }

data StateField = StateField
  { fieldType :: TypeSpec,
    fieldOrigName :: String,
    fieldName :: String,
    fieldInit :: Maybe Initializer,
    fieldScope :: Maybe String,
    fieldArraySize :: [Const]
  }
  deriving (Show)

data UseRewrite = UseRewrite
  { useName :: String,
    useScope :: Maybe String,
    useExpr :: Exp
  }
  deriving (Show)

-- StateField Maybe
uniqueDummy :: [StateField] -> [StateField]
uniqueDummy sfs =
  sequence
    [ do
        n <- case fieldName sf of
          "" -> popDummy
          name -> return name
        return sf {fieldName = n}
      | sf <- sfs
    ]
    `evalState` dummyNames
  where
    userNames = S.fromList [n | StateField {fieldName = n} <- sfs, n /= ""]
    dummyNames = filter (`S.notMember` userNames) $ map (\i -> "dummy" ++ show i) [0 ..]
    popDummy = do
      ~(x : xs) <- get
      put xs
      return x

instance Plated Exp

applyRewrites :: (Data a) => [UseRewrite] -> a -> a
applyRewrites rewrites =
  template %~ rewrite \case
    Var (Id a _) _ | Just (UseRewrite {useExpr}) <- find ((a ==) . useName) rewrites -> Just useExpr
    _ -> Nothing

-- | indexExpr arr [i,j,k] is arr[i][j][k]
indexExpr :: Exp -> [String] -> Exp
indexExpr = foldl \e v -> [cexp| $exp:e[$id:v] |]

genLoopsST :: String -> [Int] -> Stm
genLoopsST fieldName bounds = genLoops bounds \vs ->
  let lhs = indexExpr [cexp| t->$id:fieldName |] vs
      rhs = indexExpr [cexp| s->$id:fieldName  |] vs
   in [cstm| $exp:lhs = $exp:rhs; |]

genLoops :: [Int] -> ([String] -> Stm) -> Stm
genLoops bounds mkBody =
  let -- indexes = [ i,j,k ... i1, j1 ... ]
      indexes = [pre : suffix | suffix <- "" : map show [1 ..], pre <- ['i' .. 'z']]
      vs = take (length bounds) indexes
      step body (v, n) = [cstm| for (int $id:v = 0; $id:v < $int:n; $id:v++) $stm:body |]
   in foldl step (mkBody vs) (zip vs bounds)

isStaticSpec :: DeclSpec -> Bool
isStaticSpec = anyOf template \case Tstatic {} -> True; _ -> False

declrHasntFun :: Decl -> Bool
declrHasntFun (DeclRoot _) = True
declrHasntFun (Ptr _ decl _) = declrHasntFun decl
declrHasntFun (Array _ _ decl _) = declrHasntFun decl
declrHasntFun (Proto {}) = False
declrHasntFun (OldProto {}) = False
declrHasntFun (BlockPtr _ decl _) = declrHasntFun decl
declrHasntFun (AntiTypeDecl _ _) = True

declTypeSpec :: DeclSpec -> TypeSpec
declTypeSpec (DeclSpec _ _ ty _) = ty
declTypeSpec _ = Tvoid noLoc

declArraySize :: Decl -> [Const]
declArraySize (Ptr _ decl _) = declArraySize decl
declArraySize (Array _ (ArraySize _ (Const sizeConst _) _) d _) = sizeConst : declArraySize d
declArraySize (Array _ _ decl _) = declArraySize decl
declArraySize (BlockPtr _ decl _) = declArraySize decl
declArraySize _ = []

fieldFromDecl :: DeclSpec -> Maybe String -> Init -> Maybe StateField
fieldFromDecl specs scope (Init ident decl _ initVal _ _)
  | declrHasntFun decl =
      let ty = declTypeSpec specs
          name = identName ident
          arraySize = declArraySize decl
       in Just
            StateField
              { fieldType = ty,
                fieldOrigName = name,
                fieldName = name,
                fieldInit = initVal,
                fieldScope = scope,
                fieldArraySize = arraySize
              }
fieldFromDecl _ _ _ = Nothing

identName :: Id -> String
identName (Id name _) = name

mkMember :: (?s :: String) => String -> Exp
mkMember name = let sname = ?s in [cexp| $id:sname->$id:name |]

mkMemberFrom :: Id -> String -> Exp
mkMemberFrom target name = [cexp| $id:target->$id:name |]

mkMemberIndexFrom :: String -> String -> Int -> Exp
mkMemberIndexFrom target name idx =
  Index
    [cexp| $id:target->$id:name |]
    (Const (IntConst (show idx) Signed (fromIntegral idx) noLoc) noLoc)
    noLoc

initToExpr :: TypeSpec -> Initializer -> Exp
initToExpr _ (ExpInitializer expr _) = expr
initToExpr ty (CompoundInitializer items _) =
  CompoundLit (Type (DeclSpec [] [] ty noLoc) (DeclRoot noLoc) noLoc) items noLoc
initToExpr _ _ = Const (IntConst "0" Signed 0 noLoc) noLoc

initExprAt :: Int -> Initializer -> Exp
initExprAt _ (ExpInitializer expr _) = expr
initExprAt idx (CompoundInitializer items _) =
  case positionalInitAt idx items of
    Just initVal -> initExprAt idx initVal
    Nothing -> Const (IntConst "0" Signed 0 noLoc) noLoc
initExprAt _ _ = Const (IntConst "0" Signed 0 noLoc) noLoc

positionalInitAt :: Int -> [(Maybe Designation, Initializer)] -> Maybe Initializer
positionalInitAt idx items =
  let positional = [initVal | (Nothing, initVal) <- items]
   in if idx >= 0 && idx < length positional
        then Just (positional !! idx)
        else Nothing

constToArrayLen :: Const -> Maybe Int
constToArrayLen (IntConst lit _ _ _) =
  case reads lit of
    [(n, _)] | n >= (0 :: Integer) -> Just (fromInteger n)
    _ -> Nothing
constToArrayLen _ = Nothing

buildStateMembers :: [StateField] -> [InitGroup]
buildStateMembers = map buildDecl
  where
    buildDecl StateField {..} =
      let declSpec = DeclSpec [] [] fieldType noLoc
          declRoot = foldl (\y x -> Array [] (ArraySize False (Const x noLoc) noLoc) y noLoc) (DeclRoot noLoc) fieldArraySize
          initDecl = Init (mkIdent fieldName) declRoot Nothing Nothing [] noLoc
       in InitGroup declSpec [] [initDecl] noLoc

mkIdent :: String -> Id
mkIdent name = Id name noLoc
