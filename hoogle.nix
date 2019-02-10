
let
    pkgs = (import ./dependencies.nix).dependencies.nixos;
    inherit (pkgs) lib haskellPackages stdenv haskell ghc;
    haskellDev =
      haskellPackages.override ( old: {
        overrides = lib.composeExtensions
                      (old.overrides or (_: _: { }))
                      (self: super: {
                        ghc             = super.ghc // { withPackages = super.ghc.withHoogle; };
                        ghcWithPackages = self.ghc.withPackages;
                        isobmff         = haskell.lib.dontBenchmark
                                             (haskell.lib.dontCheck
                                               (import ./default.nix { inherit pkgs; }));
                        });
                      });
    ghcWithIsobmff = haskellDev.ghcWithPackages (packageList: with packageList; [isobmff]);
in
stdenv.mkDerivation {
  name = "${haskellDev.isobmff.name}-hoogle";
  buildInputs = [ ghcWithIsobmff
                  haskellDev.cabal-install
                  haskellDev.hoogle
                ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
