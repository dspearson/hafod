{
  description = "hafod - Chez Scheme port of scsh";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f {
        pkgs = nixpkgs.legacyPackages.${system};
      });
    in
    {
      packages = forAllSystems ({ pkgs }: {
        default = pkgs.stdenv.mkDerivation {
          pname = "hafod";
          version = "1.1.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.chez pkgs.makeWrapper ];
          buildInputs = [ pkgs.lz4 pkgs.zlib pkgs.ncurses ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.libiconv ];

          LDFLAGS = builtins.concatStringsSep " " (map (p: "-L${p}/lib") ([
            pkgs.lz4 pkgs.zlib pkgs.ncurses
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.libiconv ]));
          CFLAGS = builtins.concatStringsSep " " (map (p: "-I${p}/include") ([
            pkgs.lz4 pkgs.zlib pkgs.ncurses
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.libiconv ]));

          buildPhase = ''
            make compile-wpo SCHEME=${pkgs.chez}/bin/scheme
          '';

          installPhase = ''
            mkdir -p $out/lib/hafod/src $out/bin $out/share/man/man1

            # Copy library sources and compiled files
            cp -r src/hafod src/hafod.ss $out/lib/hafod/src/
            find src -name '*.so' -exec sh -c '
              for f; do
                dir="$out/lib/hafod/$(dirname "$f")"
                mkdir -p "$dir"
                cp "$f" "$dir/"
              done
            ' _ {} +

            # Install pre-compiled launcher program
            install -m 644 bin/hafod.so $out/lib/hafod/hafod.so

            # Symlink petite.boot from Chez
            ln -s ${pkgs.chez}/lib/csv*/*/petite.boot $out/lib/hafod/petite.boot

            # Create wrapper that uses --program with the compiled .so
            cat > $out/bin/hafod << 'WRAPPER'
            #!/bin/sh
            exec @chez@/bin/scheme --libdirs @out@/lib/hafod/src --program @out@/lib/hafod/hafod.so "$@"
            WRAPPER
            substituteInPlace $out/bin/hafod \
              --replace-fail '@chez@' '${pkgs.chez}' \
              --replace-fail '@out@' "$out"
            chmod +x $out/bin/hafod

            # Install man page
            install -m 644 doc/hafod.1 $out/share/man/man1/hafod.1
          '';

          meta = with pkgs.lib; {
            description = "Chez Scheme port of scsh (the Scheme Shell)";
            license = licenses.isc;
            platforms = platforms.unix;
            mainProgram = "hafod";
          };
        };
      });

      devShells = forAllSystems ({ pkgs }:
        let
          libs = [ pkgs.lz4 pkgs.zlib pkgs.ncurses ]
            ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.libiconv ];
        in {
        default = pkgs.mkShell {
          packages = [ pkgs.chez ] ++ libs;
          LDFLAGS = builtins.concatStringsSep " " (map (p: "-L${p}/lib") libs);
          CFLAGS = builtins.concatStringsSep " " (map (p: "-I${p}/include") libs);
        };
      });
    };
}
