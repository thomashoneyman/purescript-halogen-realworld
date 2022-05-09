{
  description = "RealWorld spec in the PureScript Halogen framework";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    easy-purescript-nix = {
      # when it’s time for v0.15.x, rev should be removed
      url =
        "github:justinwoo/easy-purescript-nix?rev=0ad5775c1e80cdd952527db2da969982e39ff592";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-purescript-nix, ... }@inputs:
    let
      name = "halogen-realworld";

      supportedSystems = [ "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
    {
      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };

          easy-ps = import easy-purescript-nix { inherit pkgs; };
        in
        pkgs.mkShell {
          inherit name;
          buildInputs = (with pkgs; [
            nodejs-16_x
            nixpkgs-fmt
          ]) ++ (with easy-ps; [
            purs
            purs-tidy
            psa
            spago
            purescript-language-server
          ]) ++ (pkgs.lib.optionals (system == "aarch64-darwin")
            (with pkgs.darwin.apple_sdk.framework; [
              Cocoa
              CoreServices
            ]));
        });
    };
}
