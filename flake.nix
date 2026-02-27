{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";  # pin version here

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    emPkgs = pkgs.pkgsBuildHost.emscripten;  # or pkgs.emscripten directly

    raygui-header = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/raysan5/raygui/refs/heads/master/src/raygui.h";
      sha256 = "...";
    };

    # Native build
    nativeBin = pkgs.stdenv.mkDerivation {
      name = "myapp-native";
      src = ./.;
      buildInputs = [ pkgs.raylib pkgs.ghc pkgs.cabal-install ];
      buildPhase = "gcc main.c -lraylib -o myapp";
      installPhase = "install -Dm755 myapp $out/bin/myapp";
    };

    # WASM build — raylib must be compiled with emscripten
    raylibWasm = pkgs.emscriptenStdenv.mkDerivation {
      name = "raylib-wasm";
      src = pkgs.raylib.src;  # gets the pinned raylib source
      buildInputs = [ pkgs.emscripten ];
      buildPhase = "emmake make PLATFORM=PLATFORM_WEB";
      installPhase = "install -Dm644 src/libraylib.a $out/lib/libraylib.a";
    };

    wasmBin = pkgs.emscriptenStdenv.mkDerivation {
      name = "myapp-wasm";
      src = ./.;
      buildInputs = [ raylibWasm ];
      buildPhase = ''
        cp ${raygui-header} raygui.h
        emcc main.c -L${raylibWasm}/lib -lraylib \
          -s USE_GLFW=3 -s ASYNCIFY -o myapp.html
      '';
      installPhase = "install -Dm644 myapp.* $out/www/";
    };

  in {
    packages.x86_64-linux = { native = nativeBin; wasm = wasmBin; };
    devShells.x86_64-linux.default = pkgs.mkShell {
      packages = [ pkgs.emscripten pkgs.raylib pkgs.ghc pkgs.cabal-install ];
    };
  };
}
