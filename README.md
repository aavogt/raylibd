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

 - [ ] free functions are not copied to dll.c
 - [ ] raylib functions cause parse errors, but I also can't parse raylib's headers with language-c
 - [ ] skip constants
 - [ ] fixed size arrays input is static int x[2] = {12, 34}, output includes s->x[0] = 12; s->x[1] = 34; also don't add the original decl to Step() to compile main_arrays.c
