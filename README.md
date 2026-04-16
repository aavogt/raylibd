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
- [ ] LoadFontEx, LoadAudioStream take extra arguments to be stored in AssetSlot
- [ ] rewriteAssetLoads is wrong with `Shader sh[2] = { LoadShader(vs, fs); } ;`
- [ ] hpc coverage
- [ ] integration tests
- [ ] raylibd init choose minimal minimal shader etc.?
- [ ] syz might fix scoping, make the translation easier, avoid all the intermediate state....

dll.c: In function ‘ReinitAlloc’:
dll.c:290:13: error: ‘struct prevstate’ has no member named ‘w’
  290 |     t->w = s->w;
      |             ^~
dll.c:291:15: error: ‘struct prevstate’ has no member named ‘h’; did you mean ‘sh’?
  291 |     t->h = s->h;
      |               ^
      |               sh
dll.c: In function ‘ReinitInPlace’:
dll.c:304:13: error: ‘struct prevstate’ has no member named ‘w’
  304 |     t->w = s->w;
      |             ^~
dll.c:305:15: error: ‘struct prevstate’ has no member named ‘h’; did you mean ‘sh’?
  305 |     t->h = s->h;
      |               ^


- [ ] I went to far putting variables in main() into state. It goes wrong here:

      if (ffmpeg_pipe) {
        Image frame = LoadImageFromTexture(rtA.texture);
        ImageFlipVertical(&frame); // raylib FBOs are flipped

        fwrite(frame.data, 1, vidw * vidh * 4, ffmpeg_pipe);
        UnloadImage(frame);
      }

      if (s->ffmpeg_pipe) {
          Image frame = LoadImageFromTexture(s->rtA.texture);
          
          ImageFlipVertical(&s->frame);
          fwrite(s->frame.data, 1, s->vidw * s->vidh * 4, s->ffmpeg_pipe);
          UnloadImage(s->frame);
      }

  so I have to treat variables only defined/modified inside while() differently
