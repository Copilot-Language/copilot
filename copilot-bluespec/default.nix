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

  copilot-bluespec-base =
    (hp.callCabal2nix "copilot-bluespec" (np.lib.sourceByRegex ./. sourceRegexes) {}
    ).overrideAttrs(oa: {
      nativeBuildInputs = [ np.bluespec np.removeReferencesTo ];
    });
  copilot-bluespec-overlay = _n: _o: { copilot-bluespec = copilot-bluespec-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-bluespec-overlay ];
  });
  copilot-bluespec = hp.copilot-bluespec;
in {
  inherit np;
  inherit hp;
  inherit copilot-bluespec;
}
