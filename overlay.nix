self: super:
let
  hlib = super.haskell.lib;
  git = import (import ./nix/sources.nix).gitignore {};
  prepolysemyOverrides = selfh: superh: {
    prelude-polysemy = hlib.dontHaddock
      (superh.callCabal2nix "prelude-polysemy" (git.gitignoreSource ./.) { });
  };
in {
  haskellPackages = super.haskellPackages.override (old: {
    overrides = self.lib.composeExtensions (old.overrides or (_: _: { }))
      prepolysemyOverrides;
  });
}
