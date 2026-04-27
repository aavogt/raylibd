# Raylibd

raylibd hot-reloads c raylib programs

## install

Get ghc-9.6.7 and cabal-install 3.14.1.1 or similar from
https://www.haskell.org/ghcup/install/

    git clone https://github.com/aavogt/raylibd
    cd raylibd
    cabal install

## use

    raylibd init my_app -i 10 # use example init/10-*
    cd my_app
    make -j12 # downloads raygui&raylib (if missing) into vendor/raylib/, and runs main_hot

The my_app is then a minimal fragment shader example:

      my_app
      ├── 10-tint.fs
      ├── compile_commands.json  # for clangd
      ├── dll.c
      ├── dll.so
      ├── main.c
      ├── main_hot
      ├── main_hot.c
      ├── Makefile
      └── vendor
          ├── Makefile
          └── raygui.h

The default `make watch` target calls
raylibd to turn `main.c` into `dll.c`. `main_hot` loads the `dll.so` which
knows how to reload assets (including shaders), and to migrate state.
Save changes to `main.c` or `10-tint.fs` and you should be able to see the effects right away.

## TODO

 - [ ] remove unused original variable declarations copied into dll.c
 - [ ] #line pragmas
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
- [ ] `nix build .#wasm; cd result/www; emrun myapp.html` works but maybe `Makefile`s could work inside nix shell nix develop etc.
- [ ] LoadFontEx, LoadAudioStream take extra arguments to be stored in AssetSlot
- [ ] rewriteAssetLoads is wrong with `Shader sh[2] = { LoadShader(vs, fs); } ;`
- [ ] hpc coverage
- [ ] integration tests
- [ ] name anonymous structs

      dll.c: In function ‘ReinitAlloc’:
      dll.c:284:19: error: incompatible types when assigning to type ‘struct <anonymous>’ from type ‘struct <anonymous>’
        284 |     t->entities = s->entities;

      struct {
        float x[NSTORE];
        float *y, *z;
      } store;

      vs the workaround:
      typedef struct { float x[NSTORE]; } Store;
      Store store;
