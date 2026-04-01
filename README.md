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
- [ ] `void f() {}` triggers raylibd: Transform/Sub.hs:145:1-67: Non-exhaustive patterns in function funcParams which should be like `void f(void) {}`
