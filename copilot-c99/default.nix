{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
  };
  np = import sources.nixpkgs {};
  bhp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^tests.*$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  copilot-c99-base = hp.callCabal2nix "copilot-c99" (np.lib.sourceByRegex ./. sourceRegexes) {};
  copilot-c99-overlay = _n: _o: { copilot-c99 = copilot-c99-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-c99-overlay ];
  });
  copilot-c99 = hp.copilot-c99;
in {
  inherit np;
  inherit hp;
  inherit copilot-c99;
}
