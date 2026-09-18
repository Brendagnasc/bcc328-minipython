{
  description = "Compilador MiniPython - BCC328 - DECOM/UFOP";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        # Versao de GHC exigida pelo esqueleto da disciplina (base 4.18.2.1).
        hs = pkgs.haskell.packages.ghc966;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            hs.ghc
            pkgs.cabal-install
            hs.alex
            hs.happy
            hs.haskell-language-server
          ];

          shellHook = ''
            echo "MiniPython: $(ghc --version)"
          '';
        };
      });
}
