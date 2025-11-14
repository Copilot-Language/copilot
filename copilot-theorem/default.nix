{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
    copilot-prettyprinter = (import ../copilot-prettyprinter { inherit sources ghc; }).copilot-prettyprinter;
    what4 = bhp.what4_1_7;
  };
  np = import sources.nixpkgs {};
  bhp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^tests.*$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  importZ3 = drv:
    drv.overrideAttrs (oa: {
      propagatedBuildInputs = (oa.propagatedBuildInputs or []) ++ [np.z3];
    });

  copilot-theorem-base = importZ3 (hp.callCabal2nix "copilot-theorem" (np.lib.sourceByRegex ./. sourceRegexes) {});
  copilot-theorem-overlay = _n: _o: { copilot-theorem = copilot-theorem-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-theorem-overlay ];
  });
  copilot-theorem = hp.copilot-theorem;
in {
  inherit np;
  inherit hp;
  inherit copilot-theorem;
}
