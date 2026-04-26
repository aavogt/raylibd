{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module GuessTD where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as C8
import Data.List
import Language.LBNF
import PyF
import System.Directory
import Data.Functor

bnfc
  [lbnf|
comment "//";
comment "/*" "*/";

token Number digit+;

entrypoints Program;

Program. Program ::= [Item];
separator Item "";

TypedefStruct. Item ::= "typedef" "struct" "{" [Field] "}" Ident ";";
TypedefStructNoSemi. Item ::= "typedef" "struct" "{" [Field] "}" Ident;
TypedefStructTag. Item ::= "typedef" "struct" Ident "{" [Field] "}" Ident ";";
TypedefStructTagNoSemi. Item ::= "typedef" "struct" Ident "{" [Field] "}" Ident;
TypedefAlias. Item ::= "typedef" Type Declarator ";";
FunDecl. Item ::= Type Ident "(" [Param] ")" ";";
DeclInit. Item ::= Type Declarator "=" Initializer ";";
Decl. Item ::= Type Declarator ";";

separator Field "";
InitializerIdent. Initializer ::= Ident;
InitializerNumber. Initializer ::= Number;
InitializerTrue. Initializer ::= "true";
InitializerFalse. Initializer ::= "false";
Field. Field ::= Type Declarator ";";

separator Param ",";
Param. Param ::= Type Declarator;
ParamOnly. Param ::= Type;

Void. Type ::= "void";
Char. Type ::= "char";
Short. Type ::= "short";
Int. Type ::= "int";
Long. Type ::= "long";
Float. Type ::= "float";
Double. Type ::= "double";
Signed. Type ::= "signed";
Unsigned. Type ::= "unsigned";
StructAnon. Type ::= "struct" "{" [Field] "}";
StructAnonTag. Type ::= "struct" Ident "{" [Field] "}";
EnumAnon. Type ::= "enum" "{" [EnumItem] "}";
EnumAnonTag. Type ::= "enum" Ident "{" [EnumItem] "}";
EnumTag. Type ::= "enum" Ident;
StaticType. Type ::= "static" Type;
ConstType. Type ::= "const" Type;
ExternType. Type ::= "extern" Type;
RegisterType. Type ::= "register" Type;
AutoType. Type ::= "auto" Type;
RestrictType. Type ::= "restrict" Type;
TypeIdent. Type ::= Ident;

separator EnumItem ",";
EnumItem. EnumItem ::= Ident;
EnumItemVal. EnumItem ::= Ident "=" Number;

DIdent. Declarator ::= Ident;
DAnonPtr. Declarator ::= "*";
DPtr. Declarator ::= "*" Declarator;
DArray. Declarator ::= Declarator "[" "]";
DArraySized. Declarator ::= Declarator "[" Number "]";
|]


e1 = [str|
    typedef struct {
      Vector2 p[2];
      bool placed[2];
      bool sel[3];
      enum { NODRAG = 0, P0, P1, LINE } dragging;
      Vector2 m;
    } Seg;

    typedef struct {
      Seg *entries;
      size_t count;
      size_t capacity;
    } Segs
    Segs segs;

    void f(X * a) { a->b; "}"  }

    const T2 a;
    static T3 b;
    T4 y[23] = {1,2,3}, z = 4;
    bool poly_has_anchor = false;
  |]

test1 :: IO Bool
test1 = do
  let (err, tds) = guessTypeDefs (C8.pack e1)
      eqset a b = null (a\\b) && null (b \\a)
      success = tds `eqset` words "Vector2 bool Seg size_t Segs T2 T3 T4 X"
  unless (null err) $ print ("err", err)
  print tds
  return success

test2 :: IO Bool
test2 = do
  inis <- getDirectoryContents "init" <&> filter (".c" `isSuffixOf`)
  err <- mapM (\f -> do
      print f
      (err, r) <- guessTypeDefs <$> C8.readFile ("init/"++f)
      print (err, r)
      return err) inis
  return (all null err)

guessTypeDefs :: C8.ByteString -> (String, [String])
guessTypeDefs cfile =
  case pProgram (myLexer input) of
    Bad msg -> (msg, [])
    Ok (Program items) -> ("", nub (concatMap itemNames items))
  where
    input = stripFunctionBodies (stripInitializers (stripCommentsAndCpp (C8.unpack cfile)))

stripInitializers :: String -> String
stripInitializers = go False False False 0 0
  where
    go _ _ _ _ _ [] = []
    go inString inChar escaped braceDepth parenDepth (c:cs)
      | inString =
          let escaped' = not escaped && c == '\\'
              inString' = if not escaped && c == '"' then False else True
           in c : go inString' inChar escaped' braceDepth parenDepth cs
      | inChar =
          let escaped' = not escaped && c == '\\'
              inChar' = if not escaped && c == '\'' then False else True
           in c : go inString inChar' escaped' braceDepth parenDepth cs
      | c == '"' = c : go True False False braceDepth parenDepth cs
      | c == '\'' = c : go False True False braceDepth parenDepth cs
      | c == '{' = c : go False False False (braceDepth + 1) parenDepth cs
      | c == '}' = c : go False False False (max 0 (braceDepth - 1)) parenDepth cs
      | c == '(' = c : go False False False braceDepth (parenDepth + 1) cs
      | c == ')' = c : go False False False braceDepth (max 0 (parenDepth - 1)) cs
      | c == '='
        , braceDepth == 0
        , parenDepth == 0 =
          let rest = skipInitializer 0 0 cs
           in ';' : go False False False braceDepth parenDepth rest
      | c == ','
        , braceDepth == 0
        , parenDepth == 0 =
          let rest = skipInitializer 0 0 cs
           in ';' : go False False False braceDepth parenDepth rest
      | otherwise = c : go False False False braceDepth parenDepth cs

    skipInitializer _ _ [] = []
    skipInitializer braceDepth parenDepth (c:cs)
      | c == '"' = skipInitializer braceDepth parenDepth (skipString cs)
      | c == '\'' = skipInitializer braceDepth parenDepth (skipChar cs)
      | c == '{' = skipInitializer (braceDepth + 1) parenDepth cs
      | c == '}' = skipInitializer (max 0 (braceDepth - 1)) parenDepth cs
      | c == '(' = skipInitializer braceDepth (parenDepth + 1) cs
      | c == ')' = skipInitializer braceDepth (max 0 (parenDepth - 1)) cs
      | c == ';'
        , braceDepth == 0
        , parenDepth == 0 = cs
      | otherwise = skipInitializer braceDepth parenDepth cs

    skipString = go False
      where
        go _ [] = []
        go escaped (c:cs)
          | not escaped && c == '\\' = go True cs
          | not escaped && c == '"' = cs
          | otherwise = go False cs

    skipChar = go False
      where
        go _ [] = []
        go escaped (c:cs)
          | not escaped && c == '\\' = go True cs
          | not escaped && c == '\'' = cs
          | otherwise = go False cs
stripCommentsAndCpp :: String -> String
stripCommentsAndCpp = stripBlockComments . stripLineComments . stripCppLines

stripCppLines = unlines . map dropIfCpp . lines
  where
    dropIfCpp line = case dropWhile isSpace line of
      ('#' : _) -> ""
      _ -> line

stripLineComments :: String -> String
stripLineComments = go False False False
  where
    go _ _ _ [] = []
    go inString inChar escaped (c1:c2:cs)
      | inString = c1 : go (not (not escaped && c1 == '"')) False (not escaped && c1 == '\\') (c2:cs)
      | inChar = c1 : go False (not (not escaped && c1 == '\'')) (not escaped && c1 == '\\') (c2:cs)
      | c1 == '"' = c1 : go True False False (c2:cs)
      | c1 == '\'' = c1 : go False True False (c2:cs)
      | c1 == '/' && c2 == '/' = dropUntilNewline cs
      | otherwise = c1 : go False False False (c2:cs)
    go _ _ _ [c] = [c]

    dropUntilNewline [] = []
    dropUntilNewline (c:cs)
      | c == '\n' = c : go False False False cs
      | otherwise = dropUntilNewline cs

stripBlockComments :: String -> String
stripBlockComments = go False False False
  where
    go _ _ _ [] = []
    go inString inChar escaped (c1:c2:cs)
      | inString = c1 : go (not (not escaped && c1 == '"')) False (not escaped && c1 == '\\') (c2:cs)
      | inChar = c1 : go False (not (not escaped && c1 == '\'')) (not escaped && c1 == '\\') (c2:cs)
      | c1 == '"' = c1 : go True False False (c2:cs)
      | c1 == '\'' = c1 : go False True False (c2:cs)
      | c1 == '/' && c2 == '*' = skipBlock cs
      | otherwise = c1 : go False False False (c2:cs)
    go _ _ _ [c] = [c]

    skipBlock [] = []
    skipBlock (c1:c2:cs)
      | c1 == '*' && c2 == '/' = go False False False cs
      | otherwise = skipBlock (c2:cs)

stripFunctionBodies :: String -> String
stripFunctionBodies = go Nothing 0 False False False
  where
    go _ _ _ _ _ [] = []
    go prevNonSpace parenDepth inString inChar escaped (c:cs)
      | inString =
          let escaped' = not escaped && c == '\\'
              inString' = if not escaped && c == '"' then False else True
           in c : go prevNonSpace parenDepth inString' inChar escaped' cs
      | inChar =
          let escaped' = not escaped && c == '\\'
              inChar' = if not escaped && c == '\'' then False else True
           in c : go prevNonSpace parenDepth inString inChar' escaped' cs
      | c == '"' = c : go prevNonSpace parenDepth True False False cs
      | c == '\'' = c : go prevNonSpace parenDepth False True False cs
      | c == '(' = c : go (nextPrev c prevNonSpace) (parenDepth + 1) False False False cs
      | c == ')' = c : go (nextPrev c prevNonSpace) (max 0 (parenDepth - 1)) False False False cs
      | c == '{'
        , parenDepth == 0
        , prevNonSpace == Just ')' =
          let rest = skipBlock 1 cs
           in ';' : go (Just ';') parenDepth False False False rest
      | otherwise = c : go (nextPrev c prevNonSpace) parenDepth False False False cs

    nextPrev ch prev
      | isSpace ch = prev
      | otherwise = Just ch

    skipBlock _ [] = []
    skipBlock depth (c:cs)
      | c == '"' = skipBlock depth (skipString cs)
      | c == '\'' = skipBlock depth (skipChar cs)
      | c == '{' = skipBlock (depth + 1) cs
      | c == '}' = if depth == 1 then cs else skipBlock (depth - 1) cs
      | otherwise = skipBlock depth cs

    skipString = go False
      where
        go _ [] = []
        go escaped (c:cs)
          | not escaped && c == '\\' = go True cs
          | not escaped && c == '"' = cs
          | otherwise = go False cs

    skipChar = go False
      where
        go _ [] = []
        go escaped (c:cs)
          | not escaped && c == '\\' = go True cs
          | not escaped && c == '\'' = cs
          | otherwise = go False cs

itemNames :: Item -> [String]
itemNames item =
  case item of
    TypedefStruct fields name -> fieldNames fields ++ [identName name]
    TypedefStructNoSemi fields name -> fieldNames fields ++ [identName name]
    TypedefStructTag _ fields name -> fieldNames fields ++ [identName name]
    TypedefStructTagNoSemi _ fields name -> fieldNames fields ++ [identName name]
    TypedefAlias ty decl -> typeNames ty ++ declaratorName decl
    FunDecl ty _ params -> typeNames ty ++ concatMap paramNames params
    Decl ty decl -> typeNames ty
    DeclInit ty _ _ -> typeNames ty

paramNames :: Param -> [String]
paramNames param =
  case param of
    Param ty decl -> typeNames ty
    ParamOnly ty -> typeNames ty

fieldNames :: [Field] -> [String]
fieldNames fields = concatMap fieldName fields

fieldName :: Field -> [String]
fieldName field =
  case field of
    Field ty decl -> typeNames ty

typeNames :: Type -> [String]
typeNames ty =
  case ty of
    Void -> []
    Char -> []
    Short -> []
    Int -> []
    Long -> []
    Float -> []
    Double -> []
    Signed -> []
    Unsigned -> []
    StructAnon fields -> fieldNames fields
    StructAnonTag name fields -> identName name : fieldNames fields
    EnumAnon _ -> []
    EnumAnonTag name _ -> [identName name]
    EnumTag name -> [identName name]
    StaticType inner -> typeNames inner
    ConstType inner -> typeNames inner
    ExternType inner -> typeNames inner
    RegisterType inner -> typeNames inner
    AutoType inner -> typeNames inner
    RestrictType inner -> typeNames inner
    TypeIdent name -> [identName name]

declaratorName :: Declarator -> [String]
declaratorName decl =
  case decl of
    DIdent name -> [identName name]
    DPtr inner -> declaratorName inner
    DArray inner -> declaratorName inner
    DArraySized inner _ -> declaratorName inner

identName :: Ident -> String
identName (Ident x) = x
