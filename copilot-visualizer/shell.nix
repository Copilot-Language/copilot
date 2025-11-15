{}:
let
  inherit (import ./. {}) copilot-visualizer hp np;
in
hp.shellFor {
  packages = p: [ copilot-visualizer ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]);
}
