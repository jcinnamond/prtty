{
  description = "Presentations in the tty";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
    {
      devShells."${system}".default =
        let
          pkgs = import nixpkgs {
            inherit system;
          };
          haskellPackages = pkgs.haskell.packages.ghc9122;
        in
        pkgs.mkShell {
          packages = [
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.cabal-gild
            haskellPackages.fourmolu
            haskellPackages.hlint
          ];
        };
    };
}
