let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "678070816270726e2f428da873fe3f2736201f42";
    sha256 = "13l9c1sgakpmh9f23201s8d1lnv0zz0q1wsr1lc92wdpkxs9nii4";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-realworld";
  buildInputs = with pursPkgs; [
    purs
    purs-tidy
    spago
    zephyr
    pkgs.nodejs-16_x
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    # Apple M1
    Cocoa
    CoreServices
  ]);
}
