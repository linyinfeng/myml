{ system ? builtins.currentSystem
, pkgs ? import <nixpkgs> { inherit system; }
, haskellPackages ? pkgs.haskellPackages
}:
let derivation =
  haskellPackages.callPackage ./derivation.nix
    {
      neat-interpolation = haskellPackages.neat-interpolation_0_5_1_2;
    };
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
