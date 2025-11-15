{}:
let
  inherit (import ./. {}) copilot-theorem hp np;
in
hp.shellFor {
  packages = p: [ copilot-theorem ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]);
}
