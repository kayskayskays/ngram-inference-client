{
  description = "Ngram Inference Client in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        formatter = pkgs.nixfmt;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            cabal-install
            haskell-language-server

            haskellPackages.hoogle
            fourmolu
          ];

          shellHook = ''
            echo ""
            echo "-----------------------"
            echo "GHC: $(ghc --numeric-version)"
            echo "Cabal: $(cabal --numeric-version)"
            echo "HLS: $(haskell-language-server-wrapper --numeric-version)"
            echo "-----------------------"
            echo ""
          '';
        };
      }
    );
}
