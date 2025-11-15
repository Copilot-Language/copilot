{}:
let
  inherit (import ./. {}) copilot-bluespec hp np externalDeps;
in
hp.shellFor {
  packages = p: [ copilot-bluespec ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]) ++ externalDeps;
}
