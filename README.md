# Raylibd

raylibd is for hot reloading ordinary c raylib programs.

Get ghc-9.6.7 and cabal-install 3.14.1.1 or similar from
https://www.haskell.org/ghcup/install/

    git clone https://github.com/aavogt/raylibd
    cd raylibd
    cabal install

    raylibd init demo
    cd demo
    make -j12 # downloads raygui&raylib into vendor/raylib/, and runs main_hot

Inside the demo directory:

      demo
      ├── main.c
      ├── main_hot.c
      ├── Makefile
      └── vendor
          └── Makefile

raylibd compiles `main.c` to `dll.c`, which in turn is loaded by `main_hot`
Save changes to `main.c` and you can usually see the changes right away.

## TODO

 - [ ] remove original variable declarations?
 - [ ] #line pragmas
 - [ ] hygiene

        // Calling the parameter drag makes it become s_top->drag
        int ndrag(Drag *pdrag) {
          int n = 0;
          for (int i = 0; i < 5; i++) {
            if (!(pdrag->segs[i]))
              n++;
          }
          return n;
        }

        void dragseg(Seg *seg) {
          static Drag drag;
          // start end etc.
          // if (ndrag(&drag)
          for (int j = 0; j < 2; j++) {
            if (seg->sel[j] && seg->placed[j]) {
              printf("moving a %d\n", j);
              seg->p[j] = GetMousePosition();
            }
          }

        }
- [ ] nested arrays could reuse more space?

      struct state {
      int nframe;
      int n;
      int m;
      int x;
      int xs[12][2];

      };
      struct prevstate {
      int nframe;
      int n;
      int m;
      int x;
      int dummy0[12][2];
      int dummy1[13][2];
      int xs[14][2];

      };
  - [ ] indexes are out of order:

              void ReinitInPlace(struct prevstate *s, struct state *t) {
          {
              for (int j = 0; j < 12; j++)
                  for (int i = 0; i < 2; i++)
                      t->xs[i][j] = s->xs[i][j];
          }

          dll.c: In function ‘ReinitInPlace’:
          dll.c:85:35: warning: iteration 2 invokes undefined behavior [-Waggressive-loop-optimizations]
            85 |             t->xs[i][j] = s->xs[i][j];
                |                           ~~~~~~~~^~~
          dll.c:83:23: note: within this loop
            83 |     for (int j = 0; j < 12; j++)

