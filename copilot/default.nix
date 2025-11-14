{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
    copilot-c99 = (import ../copilot-c99 { inherit sources ghc; }).copilot-c99;
    copilot-language = (import ../copilot-language { inherit sources ghc; }).copilot-language;
    copilot-libraries = (import ../copilot-libraries { inherit sources ghc; }).copilot-libraries;
    copilot-prettyprinter = (import ../copilot-prettyprinter { inherit sources ghc; }).copilot-prettyprinter;
    copilot-theorem = (import ../copilot-theorem { inherit sources ghc; }).copilot-theorem;
  };
  np = import sources.nixpkgs {};
  bhp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^examples.*$"
    "^runtest$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  withExamples = drv: drv.overrideAttrs(oa:
    { configureFlags = oa.configureFlags ++ [ "-f examples" ]; });

  copilot-base = withExamples (hp.callCabal2nix "copilot" (np.lib.sourceByRegex ./. sourceRegexes) {});

  copilot-overlay = _n: _o: { copilot = copilot-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-overlay ];
  });
  copilot = hp.copilot;
in {
  inherit np;
  inherit hp;
  inherit copilot;
}
