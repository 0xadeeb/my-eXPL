{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "expl-shell";
  packages = [  ];
  shellHook = "";
  buildInputs = with pkgs; [ 
    rust-analyzer
    rustup
  ];
}

