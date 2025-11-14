{}:
let
  inherit (import ./. {}) copilot-verifier hp np;
in
hp.shellFor {
  packages = p: [ copilot-verifier ];
  nativeBuildInputs = (with np; [ cabal-install ghcid niv np.clang_16 np.llvm_16 np.z3 ]);
}
