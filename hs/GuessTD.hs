{-# LANGUAGE QuasiQuotes #-}

module GuessTD (guessTD, guessTDloc) where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.Functor
import Data.List
import qualified Data.Map.Strict as M
import Data.Loc (SrcLoc)
import GuessTD.CollectTypeNames
import GuessTD.Lex
import PyF
import System.Directory

-- | guess typedefs
guessTD :: C8.ByteString -> (String, [String])
guessTD cfile =
  let (err, names) = guessTDloc cfile
   in (err, map fst names)

guessTDloc :: C8.ByteString -> (String, [(String, SrcLoc)])
guessTDloc cfile =
  let input = C8.unpack cfile
      toks = collapseParens (collapseBlocks (lexTokens input))
      names = M.filterWithKey (\name _ -> not (isBuiltin name)) (collectTypeNames (splitStatements toks))
   in ("", M.toList names)

e1 =
  [str|
    typedef struct Entity {
      int line;
      enum { Entity_NONE, Entity_POINT, Entity_SEG, Entity_CONSTRAINT } tag;
      union {
        Vector2 point;
        struct {
          struct Entity *a, *b;
        } seg;
        struct {
          ConstraintKind kind;
          int argc;
          Entity *args[MAX_CARGS]; // array not supported yet
          float value;
        } constraint;
        C2 (*make)(C1 *); // function pointer field
        C3 (*combine)(C4, C5 *); // function pointer field
        C6 (*callbacks[2])(C7 *); // array of callbacks
        C8 (*(*factory)(C9))(C10 *); // returns pointer to function
      };
    } Entity;

    typedef struct {
      Vector2 p[2];
      bool placed[2];
      bool sel[3];
      enum { NODRAG = 0, P0, P1, LINE } dragging;
      Vector2 m;
    } Seg;

    typedef struct {
      Seg2 *y, *z;
    } Segs2;
    Entities entities;

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

    bool h() {
      int32_t R = 0, G = 0, B = 0;
      return a || b;
    }

    typedef struct B1 B2;
    typedef B1 *(*B2)(B3 *a);
    typedef B4 (*B5)(B6 b, B7 *c);
    typedef B8 *(*B9)(B10 *a);
    typedef B11 (*(*B12)(B13 b))(B14 *c);

  int main() {
    SetConfigFlags(FLAG_MSAA_4X_HINT | FLAG_WINDOW_RESIZABLE | FLAG_VSYNC_HINT);
    InitWindow(800, 600, "main.c");
    SetTargetFPS(144);
    while (!WindowShouldClose()) {
      static float time = 0;
      static bool up = true;
      if (time * 10 > 512) {
        up = false;
        time = 51;
      }
      if (time <= 0) {
        up = true;
        time = 0;
      }
      if (up)
        time += 10 * GetFrameTime();
      else
        time -= 10 * GetFrameTime();
      lut_init();
      render_frame(time * 10);
      BeginDrawing();
      ClearBackground(BLACK);
      draw_pixels();
      EndDrawing();
    }
    CloseWindow();
  }

  float cy = fmaxf(py, fminf(ball.y, py + paddlew));
  |]

eqset a b = null (a \\ b) && null (b \\ a)

test1 :: IO Bool
test1 = do
  let (err, tds) = guessTD (C8.pack e1)
      success = tds `eqset` expected
      expected =
        words
          "ConstraintKind Vector2 bool Seg size_t \
          \Segs Seg2 Segs2 T2 T3 T4 Entities Entity X int32_t"
          ++ ["B" ++ show n | n <- [1 .. 14]]
          ++ ["C" ++ show n | n <- [1 .. 10]]
  unless success $ print ("err", err, tds \\ expected, expected \\ tds)
  print tds
  return success

test2 :: IO Bool
test2 = do
  inis <- getDirectoryContents "init" <&> filter (".c" `isSuffixOf`)
  err <-
    mapM
      ( \f -> do
          print f
          (err, r) <- guessTD <$> C8.readFile ("init/" ++ f)
          print (err, r)
          return err
      )
      inis
  return (all null err)

test3 :: IO Bool
test3 = do
  ("", s) <- return $ guessTD $ C8.pack "typedef struct { union { Vector2 point; }; } X;"
  print s
  return (s `eqset` words "Vector2 X")
