{ pkgs ? import <nixpkgs> {}}:

pkgs.haskellPackages.shellFor {
  packages = hps: [
    (hps.callCabal2nix "hasql-pool" (pkgs.nix-gitignore.gitignoreSource [".git"] ./.) {})
  ];
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}
