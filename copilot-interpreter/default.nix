{ sources ? import ../nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
    copilot-prettyprinter = (import ../copilot-prettyprinter { inherit sources ghc; }).copilot-prettyprinter;
  };
  np = import sources.nixpkgs {};
  bhp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^tests.*$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  copilot-interpreter-base = hp.callCabal2nix "copilot-interpreter" (np.lib.sourceByRegex ./. sourceRegexes) {};
  copilot-interpreter-overlay = _n: _o: { copilot-interpreter = copilot-interpreter-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-interpreter-overlay ];
  });
  copilot-interpreter = hp.copilot-interpreter;
in {
  inherit np;
  inherit hp;
  inherit copilot-interpreter;
}
