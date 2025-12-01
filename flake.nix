{
  description = "Advent of Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, rust-overlay, ... }:
    let
      lib = nixpkgs.lib;
      systems = [ "x86_64-linux" ];

      forEachSystem =
        fn:
        lib.genAttrs systems (
          system:
          let
            overlays = [ (import rust-overlay) ];
            pkgs = import nixpkgs { inherit system overlays; };
          in
          fn { inherit pkgs; }
        );

      mkRustToolchain = pkgs: pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
    in
    {
      devShells = forEachSystem (
        { pkgs }:
        let
          rustToolchain = mkRustToolchain pkgs;
          rustNightlyToolchain = pkgs.rust-bin.selectLatestNightlyWith (
            t:
            t.minimal.override {
              extensions = [ "rustfmt" ];
            }
          );

          rustShell = pkgs.mkShell {
            name = "AoC";

            nativeBuildInputs = with pkgs; [
              rustToolchain
              rustNightlyToolchain
              just
            ];
          };
        in
        {
          default = rustShell;
        }
      );
    };
}
