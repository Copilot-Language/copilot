{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
    copilot-interpreter = (import ../copilot-interpreter { inherit sources ghc; }).copilot-interpreter;
    copilot-theorem = (import ../copilot-theorem { inherit sources ghc; }).copilot-theorem;
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

  copilot-language-base = hp.callCabal2nix "copilot-language" (np.lib.sourceByRegex ./. sourceRegexes) {};
  copilot-language-overlay = _n: _o: { copilot-language = copilot-language-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-language-overlay ];
  });
  copilot-language = hp.copilot-language;
in {
  inherit np;
  inherit hp;
  inherit copilot-language;
}
