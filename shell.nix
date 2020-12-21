let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.ghc
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.ormolu
  ];
}

