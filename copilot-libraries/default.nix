{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot-language = (import ../copilot-language { inherit sources ghc; }).copilot-language;
    copilot-interpreter = (import ../copilot-interpreter { inherit sources ghc; }).copilot-interpreter;
    copilot-theorem = (import ../copilot-theorem { inherit sources ghc; }).copilot-theorem;
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

  copilot-libraries-base =
    importZ3 (hp.callCabal2nix "copilot-libraries" (np.lib.sourceByRegex ./. sourceRegexes) {});

  copilot-libraries-overlay = _n: _o: { copilot-libraries = copilot-libraries-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-libraries-overlay ];
  });
  copilot-libraries = hp.copilot-libraries;
in {
  inherit np;
  inherit hp;
  inherit copilot-libraries;
}
