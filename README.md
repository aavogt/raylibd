# Raylibd

raylibd hot-reloads c raylib programs

## install

Get ghc-9.6.7 and cabal-install 3.14.1.1 or similar from
https://www.haskell.org/ghcup/install/

    git clone https://github.com/aavogt/raylibd
    cd raylibd
    cabal install

## use

    raylibd init my_app
    cd my_app
    make -j12 # downloads raygui&raylib into vendor/raylib/, and runs main_hot

Inside the demo directory:

      my_app
      ├── main.c
      ├── main_hot.c
      ├── Makefile
      └── vendor
          └── Makefile

raylibd compiles `main.c` to `dll.c`, which in turn is loaded by `main_hot`
Save changes to `main.c` and you should be able to see the effects right away.

The initial main.c is a 2d simulation with two masses connected with an elastic
bar with some damping driven bounces off the walls. You can try it in your
browser [here](http://aavogt.github.io/sim), but unfortunately the wasm version
is missing hot reloading.

## TODO

 - [ ] remove unused original variable declarations copied into dll.c
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
- [ ] `nix build .#wasm; cd result/www; emrun myapp.html` works but maybe `Makefile`s could work inside nix shell nix develop etc.
- [ ] `main () { RenderTexture2D rtA = f(); while(true){ rtA; }}` Step() has an undefined rtA. The workaround is `static RenderTexture2D rtA; rtA = f();`
- [ ] shaders, textures, models aren't even watched

### Asset & Shader Hot-Reload Plan option 1. more haskell


String literals in `main.c` with known extensions (`.fs`, `.vs`, `.glsl`, `.png`, `.obj`, `.gltf`, `.ttf`, `.wav`, etc.) are extracted from the AST and written to `dll.assets`.

**Phase A** — `Transform/Assets.hs`: traverse the AST for `StringConst` nodes, filter by extension, return `[(var_name, scope, path, load_call)]`.

**Phase B** — `hs/main.hs`: after writing `dll.c`, also write `dll.assets` (one path per line).

**Phase C** — `main_hot.c`: on dll reload, read `dll.assets`; poll `stat()` each asset path every N frames.

**Phase D** — `template.c` + codegen: add `ReloadAsset(struct state *s, const char *path)` to VTable. Generated body does `strcmp` on each known path and calls the matching `Unload*/Load*` pair.

**Phase E** — shader vs asset: shaders (`.fs`/`.vs`/`.glsl`) → `UnloadShader` + `LoadShader`; textures → `UnloadTexture` + `LoadTexture`; etc. No gcc recompile needed — just runtime reload inside the dll.

Polling happens in `main_hot.c` alongside the existing `dll.so` mtime check. Asset list refreshes on each dll reload.

### Asset & Shader Option 2 more c

raylibd rewrites LoadShader to LoadShader2, LoadTexture to LoadTexture2 etc.. 
These register their arguments and results in struct state.
