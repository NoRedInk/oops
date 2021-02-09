let pkgs = import nix/packages.nix;
in pkgs.mkShell {
  buildInputs = [ pkgs.pkgs.ghc pkgs.pcre pkgs.niv pkgs.oops ];
}
