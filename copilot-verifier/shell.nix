{}:
let
  inherit (import ./. {}) copilot-verifier hp np externalDeps shellTuning;
in
hp.shellFor {
  packages = p: [ copilot-verifier ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv]) ++ externalDeps;
  shellHook = shellTuning;
}
