{}:
let
  inherit (import ./. {}) copilot-language hp np;
in
hp.shellFor {
  packages = p: [ copilot-language ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]);
}
