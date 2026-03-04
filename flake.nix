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
          version = "2.1.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.chez pkgs.makeWrapper ];

          buildPhase = ''
            make compile SCHEME=${pkgs.chez}/bin/scheme
          '';

          installPhase = ''
            mkdir -p $out/lib/hafod/src $out/bin $out/share/man/man1

            # Copy library sources and compiled files
            cp -r src/hafod src/hafod.ss $out/lib/hafod/src/
            cp -r src/hafod.so $out/lib/hafod/src/ 2>/dev/null || true
            find src -name '*.so' -exec sh -c '
              for f; do
                dir="$out/lib/hafod/$(dirname "$f")"
                mkdir -p "$dir"
                cp "$f" "$dir/"
              done
            ' _ {} +

            # Install launcher script
            install -m 644 bin/hafod.sps $out/bin/hafod.sps

            # Create wrapper
            cat > $out/bin/hafod << 'WRAPPER'
            #!/bin/sh
            exec @chez@/bin/scheme --libdirs @out@/lib/hafod/src --script @out@/bin/hafod.sps "$@"
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
            license = licenses.bsd3;
            platforms = platforms.unix;
            mainProgram = "hafod";
          };
        };
      });

      devShells = forAllSystems ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            chez
          ];
        };
      });
    };
}
