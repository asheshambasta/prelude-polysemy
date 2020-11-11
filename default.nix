{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix;
  rp = import sources.reflex-platform { inherit system; };
in rp.project ({ pkgs, ... }: {

  name = "prelude-polysemy";

  # uses jsaddle-warp to fire up a warp server instead of webgtk.
  useWarp = true;
  withHoogle = false;

  packages = {
    prelude-polysemy = ./.;
  };

  shells = {
    ghc = [ "prelude-polysemy" ];
    ghcjs = [ "prelude-polysemy" ];
  }; 
})
