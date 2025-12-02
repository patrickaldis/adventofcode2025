let
  rust_overlay = import (
    builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz"
  );
  pkgs = import <nixpkgs> { overlays = [ rust_overlay ]; };
  rustVersion = "latest";
  rust = pkgs.rust-bin.stable.${rustVersion}.default.override {
    extensions = [
      "rust-src" # for rust-analyzer
      "rust-analyzer"
    ];
  };
  rustShell = pkgs.mkShell {
    buildInputs = [
      rust
    ]
    ++ (with pkgs; [
      pkg-config
      rustlings
      # other dependencies
      #gtk3
      #wrapGAppsHook
    ]);
    RUST_BACKTRACE = 1;
  };
  haskellShell = pkgs.haskellPackages.shellFor {
    packages = hpkgs: [
      (hpkgs.callCabal2nix "adventOfCode2025" ./Haskell { })
    ];
    nativeBuildInputs = with pkgs; [
      haskell-language-server
      cabal-install
    ];
  };
in
pkgs.mkShell {
  inputsFrom = [
    rustShell
    haskellShell
  ];
}
