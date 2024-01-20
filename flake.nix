{
  description = "RealWorld spec in the PureScript Halogen framework";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    purescript-overlay,
    ...
  }: let
    name = "halogen-realworld";

    supportedSystems = [
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ];

    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
  in {
    devShell = forAllSystems (system: let
      overlays = [purescript-overlay.overlays.default];
      pkgs = import nixpkgs {inherit system overlays;};
    in
      pkgs.mkShell {
        inherit name;
        buildInputs =
          [
            pkgs.esbuild
            pkgs.nodejs_20
            pkgs.nixpkgs-fmt
            pkgs.purs
            pkgs.purs-tidy
            pkgs.purs-backend-es
            pkgs.purescript-language-server
            pkgs.spago-unstable
          ]
          ++ (pkgs.lib.optionals (system == "aarch64-darwin")
            (with pkgs.darwin.apple_sdk.frameworks; [
              Cocoa
              CoreServices
            ]));
      });
  };
}
