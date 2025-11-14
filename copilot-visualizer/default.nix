{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-language = (import ../copilot-language { inherit sources ghc; }).copilot-language;
    copilot-interpreter = (import ../copilot-interpreter { inherit sources ghc; }).copilot-interpreter;
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
    copilot = (import ../copilot { inherit sources ghc; }).copilot;
    copilot-prettyprinter = (import ../copilot-prettyprinter { inherit sources ghc; }).copilot-prettyprinter;
  };
  np = import sources.nixpkgs {};
  bhp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^data.*$"
    "^examples.*$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  copilot-visualizer-base = hp.callCabal2nix "copilot-visualizer" (np.lib.sourceByRegex ./. sourceRegexes) {};
  copilot-visualizer-overlay = _n: _o: { copilot-visualizer = copilot-visualizer-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-visualizer-overlay ];
  });
  copilot-visualizer = hp.copilot-visualizer;
in {
  inherit np;
  inherit hp;
  inherit copilot-visualizer;
}
