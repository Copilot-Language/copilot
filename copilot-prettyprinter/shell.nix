{}:
let
  inherit (import ./. {}) copilot-prettyprinter hp np;
in
hp.shellFor {
  packages = p: [ copilot-prettyprinter ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv ]) ++ (with hp; [ haskell-language-server ]);
}
