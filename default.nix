{ pkgs, ... }:
let
  cleanSrc = pkgs.pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(pkgs.pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(pkgs.pkgs.lib.hasSuffix ".nix" base) &&
         !(pkgs.pkgs.lib.hasSuffix ".stack-work" base) &&
         !(pkgs.pkgs.lib.hasPrefix "dist" base)
    );
    src = pkgs.pkgs.lib.cleanSource ./.;
  };

  # Temporarily add this dependency via github
  fb = pkgs.haskellPackages.callCabal2nix
         "function-builder"
         (pkgs.fetchFromGitHub {
           owner = "sheyll";
           repo = "function-builder";
           rev = "0.3.0.1";
           sha256 = "0jimziax8ri2sly15mblcm2lys90x4x07ciw7zcys5msmk7l8ad9"; }) {};

  in pkgs.haskell.lib.doBenchmark
      (pkgs.haskellPackages.callCabal2nix
       "isobmff" cleanSrc { function-builder = fb; })
