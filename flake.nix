{
  description = "Embedding of python interpreter into haskell programs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = builtins.attrNames nixpkgs.legacyPackages;
      perSystem = attrs @ {
        system,
        pkgs,
        ...
      }: let
        pkgs = attrs.pkgs.extend self.overlays.default;

        py = pkgs.python3.withPackages (py_pkg: with py_pkg;
          [ numpy
            matplotlib
          ]);
      in {
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "inline-python-devShell";
          packages = p: [p.inline-python];
          nativeBuildInputs = with pkgs.haskellPackages; [
            pkgs.cabal-install
            cabal2nix
            haskell-language-server
          ];
          buildInputs = with pkgs; [
              py
              pkg-config
          ];

          shellHook = ''
            export PYTHONHOME=${py}
          '';
        };

        packages = rec {
          default = inline-python;
          inherit
            (pkgs.haskellPackages)
            inline-python
            ;
        };

      };
      flake = {
        overlays = {
          default = final: prev:
            with final.haskell.lib;
            with final.lib; let
              # NOTE: haskellPackages' so that we don't rebuild the world in the devShell 
              # due to the overrides
              haskellPackages' = prev.haskellPackages.override (old: {
                overrides = prev.lib.composeExtensions (old.overrides or (_: _: {})) (
                    hself: hsuper: {
                      vector = dontCheck hsuper.vector_0_13_2_0;
                      primitive = hsuper.primitive_0_9_0_0;
                      scientific = hsuper.scientific_0_3_8_0;
                      uuid-types = hsuper.uuid-types_1_0_6;
                      quickcheck-instances = hsuper.quickcheck-instances_0_3_32;

                      inline-python = (buildFromSdist (
                        overrideCabal (hsuper.callPackage ./default.nix { 
                          python3-embed = final.python3; 
                        })
                        (old: {
                          configureFlags =
                            (old.configureFlags or [])
                            ++ [
                              "--ghc-options=-O2"
                              "--ghc-options=-j"
                              "--ghc-options=+RTS"
                              "--ghc-options=-A256m"
                              "--ghc-options=-n4m"
                              "--ghc-options=-RTS"
                              "--ghc-options=-Wall"
                              # "--ghc-options=-Werror" # the current version has a bunch of warnings
                              "--ghc-options=-Wincomplete-uni-patterns"
                              "--ghc-options=-Wincomplete-record-updates"
                              "--ghc-options=-Wpartial-fields"
                              "--ghc-options=-Widentities"
                              "--ghc-options=-Wredundant-constraints"
                              "--ghc-options=-Wcpp-undef"
                              "--ghc-options=-Wunused-packages"
                              "--ghc-options=-Wno-deprecations"
                            ];
                        })
                      ));
                    }
                  );
              });
            in {
              haskellPackages = prev.haskellPackages.override (old: {
                overrides = prev.lib.composeExtensions (old.overrides or (_: _: {})) (
                  hself: hsuper: {
                      inline-python = haskellPackages'.inline-python;
                  });
              });
            };
        };
      };
    };
}
