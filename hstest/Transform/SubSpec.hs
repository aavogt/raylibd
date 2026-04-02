module Transform.SubSpec where

import Transform.Sub
import Test.Hspec
import Test.Hspec.Golden

test = do
  let fn = [cunit| void f() { while (true) { static int array[10]; } } |]
      spec = buildStateSpec fn
      mergedSF = maybe id (mergeSF . fields) Nothing (fields spec)
  pPrint fn
  pPrint $ map (pretty 100 . ppr) $ buildStateMembers mergedSF

  putStrLn $ pretty 120 $ ppr $ genLoopsST "x" [1, 5]

test2 = do
  let dl =
        [cunit| 
        typedef struct { float x, y; } Vector2;
        typedef struct {
                        Vector2 a, b;
                        typename bool placed[2];
                      } Seg;
        Seg segs[2]; 
        void f(void);
        void f(void) { printf(); }

        |]
  pPrint dl

  -- let ss0 = StateSpec [] [] (const []) []
  pPrint $ mapMaybe toDecl dl

test3 = do
  let a = [cunit| float xs[2]; char n; |]
  let b = [cunit| float xs[3]; char n; |]
  let c = [cunit| float xs[4]; char n; |]
  let d = [cunit| float xs[3]; char n; |]
  let e = [cunit| float xs[2]; char n; |]

  let sa = buildStateSpec a
  let sb = buildStateSpec b
  let sc = buildStateSpec c
  let sd = buildStateSpec d
  let se = buildStateSpec e

  let fa = fields sa
  let fb = mergeSF (fields sa) (fields sb)
  let fc = fb `mergeSF` fields sc
  let fd = fc `mergeSF` fields sd
  let fe = fd `mergeSF` fields se
  let renderDecls xs = concatMap ((++ ";\n") . render) xs
      render x = pretty 120 $ ppr x
      pp = putStrLn . renderDecls . buildStateMembers . uniqueDummy
  putStrLn "struct state_a {"
  pp fa
  putStrLn "} struct state_b {"
  pp fb
  putStrLn "} struct state_c {"
  pp fc
  putStrLn "} struct state_d {"
  pp fd
  putStrLn "} struct state_e {"
  pp fe
  putStrLn "}"

test4 = do
  let a = [cunit| float xs[2]; char n; |]
  let b = [cunit| float xs[3]; char n; |]
  let c = [cunit| float xs[4]; char n; |]
  let d = [cunit| float xs[3]; char n; |]
  let e = [cunit| float xs[2]; char n; |]

  let sa = buildStateSpec a
  let sb = buildStateSpec b
  let sc = buildStateSpec c
  let sd = buildStateSpec d
  let se = buildStateSpec e

  let fa = fields sa
  let fb = mergeSF (fields sa) (fields sb)
  let fc = fb `mergeSF` fields sc
  let fd = fc `mergeSF` fields sd
  let fe = fd `mergeSF` fields se

  let render x = pretty 120 $ ppr x
      renderBody stms = putStrLn $ render $ Block (map BlockStm stms) noLoc
      showReinit prev spec = do
        putStrLn "-- ReinitAlloc"
        renderBody (reinitAllocStmts prev spec)
        putStrLn "-- ReinitInPlace"
        renderBody (reinitInPlaceStmts prev spec)

  putStrLn "====== a->b ======"
  showReinit (Just sa {fields = fa}) (sb {fields = fb})
  putStrLn "====== b->c ======"
  showReinit (Just sb {fields = fb}) (sc {fields = fc})
  putStrLn "====== c->d ======"
  showReinit (Just sc {fields = fc}) (sd {fields = fd})
  putStrLn "====== d->e ======"
  showReinit (Just sd {fields = fd}) (se {fields = fe})

test5 = pretty 100 (ppr $ fromJust $ toDecl $ head [cunit| void f() { return; } |]) == "void f();"
