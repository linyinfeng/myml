{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> { inherit system; }
, haskellPackages ? pkgs.haskellPackages
}:
let
  derivation =
    haskellPackages.callPackage ./derivation.nix { };
in
if pkgs.lib.inNixShell
then
  derivation.env.overrideAttrs
    (oldAttrs: {
      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ (with pkgs; [
        cabal-install
      ]);
    })
else derivation
