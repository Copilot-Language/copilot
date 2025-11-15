{ sources ? import ../nix/sources.nix, ghc ? "ghc98" }:
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
in {
  inherit np;
  inherit hp;
  inherit copilot-core;
}
