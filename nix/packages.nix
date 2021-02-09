let
  sources = import ./sources.nix { };
  nivOverlay = _: pkgs: {
    niv = (import sources.niv { }).niv; # use the sources :)
  };
  oopsOverlay = _: pkgs: {
    oops = pkgs.haskellPackages.callCabal2nix "oops" ../. { };
  };
in import (sources.nixpkgs) { overlays = [ nivOverlay oopsOverlay ]; }
