{ sources ? import ./nix/sources.nix, ghc ? "ghc98" }:
let
  np = import sources.nixpkgs {};
  hp = np.haskell.packages.${ghc};

  sourceRegexes = [
    "^src.*$"
    "^tests.*$"
    "^.*\\.cabal$"
    "^LICENSE$"
  ];

  copilot-core = hp.callCabal2nix "copilot-core" (np.lib.sourceByRegex ./. sourceRegexes) {};
  # copilot-core-overlay = _n: _o: { copilot-core = copilot-core-base; };
  # hp = bhp.override (o: {
  #   overrides =
  #     builtins.foldl' np.lib.composeExtensions (o.overrides or (_: _: {}))
  #       [ copilot-core-overlay ];
  # });
  # copilot-core = hp.copilot-core;

in {
  inherit np;
  inherit hp;
  inherit copilot-core;
}
