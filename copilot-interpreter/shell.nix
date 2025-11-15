{}:
let
  inherit (import ./. {}) copilot-interpreter hp np;
in
hp.shellFor {
  packages = p: [ copilot-interpreter ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]);
}
