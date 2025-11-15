{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  depOverlay = n: o: {
    copilot = (import ../copilot { inherit sources ghc; }).copilot;
    copilot-core = (import ../copilot-core { inherit sources ghc; }).copilot-core;
    copilot-c99 = (import ../copilot-c99 { inherit sources ghc; }).copilot-c99;
    copilot-language = (import ../copilot-language { inherit sources ghc; }).copilot-language;
    copilot-interpreter = (import ../copilot-interpreter { inherit sources ghc; }).copilot-interpreter;
    copilot-libraries = (import ../copilot-libraries { inherit sources ghc; }).copilot-libraries;
    copilot-prettyprinter = (import ../copilot-prettyprinter { inherit sources ghc; }).copilot-prettyprinter;
    copilot-theorem = (import ../copilot-theorem { inherit sources ghc; }).copilot-theorem;
  };
  np = import sources.nixpkgs {};
  bhp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^exe.*$"
    "^examples.*$"
    "^test.*$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  # llvm and clang versions are coupled
  externalDeps = [np.z3 np.clang_16 np.llvm_16];
  shellTuning = ''export CCC_OVERRIDE_OPTIONS=+-fno-wrapv
  '';
  copilot-verifier-base =
    (hp.callCabal2nix "copilot-verifier" (np.lib.sourceByRegex ./. sourceRegexes) {}).overrideAttrs
      (oa: {
        checkPhase = shellTuning + oa.checkPhase;
        propagatedBuildInputs = (oa.propagatedBuildInputs or []) ++ externalDeps;
      });

  copilot-verifier-overlay = _n: _o: { copilot-verifier = copilot-verifier-base; };
  hp = bhp.override (o: {
    overrides =
      builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
        [ depOverlay copilot-verifier-overlay ];
  });
  copilot-verifier = hp.copilot-verifier;
in {
  inherit np;
  inherit hp;
  inherit externalDeps;
  inherit shellTuning;
  inherit copilot-verifier;
}
