{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";  # pin version here

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    emPkgs = pkgs.pkgsBuildHost.emscripten;  # or pkgs.emscripten directly

    raygui-header = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/raysan5/raygui/refs/heads/master/src/raygui.h";
      sha256 = "sha256-7Zz4SUSM49kgDNnT2/UKPFT+R8GQx76dihvlTz1wbCs=";
    };

    # Native build
    nativeBin = pkgs.stdenv.mkDerivation {
      name = "myapp-native";
      src = ./.;
      buildInputs = [ pkgs.raylib pkgs.ghc pkgs.cabal-install ];
      buildPhase = "gcc main.c -lraylib -o myapp";
      installPhase = "install -Dm755 myapp $out/bin/myapp";
    };

    raylibWasm = pkgs.emscriptenStdenv.mkDerivation {
      name = "raylib-wasm";
      src = pkgs.raylib.src;  # gets the pinned raylib source
      # src = pkgs.fetchurl { url = "https://github.com/raysan5/raylib/archive/refs/heads/master.tar.gz"; sha256 = "e5037cdc0504a8a27886a442a2c3a5b42fa5101ef612edffa1b40d670dcb292f"; };
      # src = pkgs.fetchurl { file = /tmp/raylib.tar.gz; sha256 = "e5037cdc0504a8a27886a442a2c3a5b42fa5101ef612edffa1b40d670dcb292f"; };
      # src = /tmp/raylib.tar.gz;
      buildInputs = [ pkgs.emscripten pkgs.cmake ];
      configurePhase = ''
        export HOME="$TMPDIR"
        export EM_CACHE="$TMPDIR/emscripten-cache"
        emcmake cmake -DPLATFORM=Web -DBUILD_EXAMPLES=OFF -Bweb
      '';
      buildPhase = ''
        export HOME="$TMPDIR"
        export EM_CACHE="$TMPDIR/emscripten-cache"
        emmake make -Cweb
      '';
      installPhase = "install -Dm644 web/raylib/libraylib.a $out/lib/libraylib.a
        mkdir $out/include
        install -Dm644 web/raylib/include/*.h $out/include/
        install -Dm644 src/minshell.html $out/
      ";
      checkPhase = [];
    };

    wasmBin = pkgs.emscriptenStdenv.mkDerivation {
      name = "myapp-wasm";
      src = ./.;
      buildInputs = [ raylibWasm ];
      configurePhase = [];
      buildPhase = ''
        export HOME="$TMPDIR"
        export EM_CACHE="$TMPDIR/emscripten-cache"
        cp ${raygui-header} raygui.h
        emcc main.c ${raylibWasm}/lib/libraylib.a -I${raylibWasm}/include \
          -s USE_GLFW=3 -s ASYNCIFY -s GL_ENABLE_GET_PROC_ADDRESS \
          --shell-file ${raylibWasm}/minshell.html \
          -o myapp.html
      '';
      installPhase = "mkdir -p $out/www; install -Dm644 myapp.* $out/www/";
      checkPhase = [];
    };

  in {
    packages.x86_64-linux = { native = nativeBin; wasm = wasmBin; };
    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = [ pkgs.emscripten pkgs.raylib pkgs.ghc pkgs.cabal-install ];
    };
  };
}
