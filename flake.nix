{
  description = "A Nix Flake providing a development environment for Chisel";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        packages = with pkgs; [
          mill
          verilator
          circt
          python3
          just
          #surferWrapped
          gtkwave
          flatbuffers
          # RISC-V cross-compilation via LLVM (no nix wrapper injection issues)
          llvmPackages_19.clang-unwrapped
          llvmPackages_19.bintools-unwrapped
          lld_19
        ];

      in {
        devShells = {
          default = pkgs.mkShell { name = "chisel"; inherit packages; };
        };
      });
}
