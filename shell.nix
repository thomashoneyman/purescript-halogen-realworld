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
    rev = "bbef4245cd6810ea84e97a47c801947bfec9fadc";
    sha256 = "00764zbwhbn61jwb5px2syzi2f9djyl8fmbd2p8wma985af54iwx";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-realworld";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pursPkgs.zephyr
    pkgs.nodejs-14_x
  ];
}
