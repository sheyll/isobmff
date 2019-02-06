{runTests ? false, tracing ? true, fullbenchmarks ? false, complextests ? false}:
with (import ./dependencies.nix).dependencies.nixos;
let
  isobmffx =
    haskell.lib.overrideCabal
      (haskellPackages.callPackage ./default.nix { })
      (old:
       {
         buildInputs = old.buildInputs ++ [
                         haskellPackages.cabal-install
                         (haskellPackages.ghcWithPackages (pl: [haskell.lib.getHaskellBuildInputs isobmffx]))
                       ];
         shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
         doCheck = runTests;
         configureFlags = old.configureFlags ++
         [ ("-f" + (if tracing then "-" else "") + "tracing")
           ("-f" + (if fullbenchmarks then "-" else "") + "fullbenchmarks")
           ("-f" + (if complextests then "-" else "") + "complextests")
         ];
       });
in
isobmffx.env
