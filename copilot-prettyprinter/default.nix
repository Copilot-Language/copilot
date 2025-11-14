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

  copilot-prettyprinter-base = hp.callCabal2nix "copilot-prettyprinter" (np.lib.sourceByRegex ./. sourceRegexes) {};
  copilot-prettyprinter-overlay = _n: _o: { copilot-prettyprinter = copilot-prettyprinter-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-prettyprinter-overlay ];
  });
  copilot-prettyprinter = hp.copilot-prettyprinter;
in {
  inherit np;
  inherit hp;
  inherit copilot-prettyprinter;
}
