let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix, run:
  # nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  #
  # Then, copy the resulting rev and sha256 here.
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "82f901ce0a2d86327e2d65993a75c2ea74f229f2";
    sha256 = "0qsq8bj76y3bxdl2iphknjib139z0jw75xlaih7viv9kvfm9b1lx";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-realworld";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.purs-tidy
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.nodejs-16_x
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    # Apple M1
    Cocoa
    CoreServices
  ]);
}
