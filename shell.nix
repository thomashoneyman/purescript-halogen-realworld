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
    rev = "0ad5775c1e80cdd952527db2da969982e39ff592";
    sha256 = "0x53ads5v8zqsk4r1mfpzf5913byifdpv5shnvxpgw634ifyj1kg";
  }) { inherit pkgs; };

in pkgs.mkShell {
  name = "halogen-realworld";
  buildInputs = [
    pursPkgs.purs
    pursPkgs.purs-tidy
    pursPkgs.psa
    pursPkgs.spago

    pkgs.nodejs-16_x
    pkgs.nixfmt
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    # Apple M1
    Cocoa
    CoreServices
  ]);
}
