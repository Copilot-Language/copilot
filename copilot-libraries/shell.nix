{}:
let
  inherit (import ./. {}) copilot-libraries hp np;
in
hp.shellFor {
  packages = p: [ copilot-libraries ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]);
}
