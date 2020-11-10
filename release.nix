let
  sources = import ./nix/sources.nix;

  mkPkgs = import sources.nixpkgs; 
  nixpkgs = mkPkgs { };

  allSources = let
    sources-generated = if builtins.pathExists ./sources-generated.nix then
      import ./sources-generated.nix
    else
      { };
  in sources // sources-generated;

  # Add all source related overlays here.
  sourcesOverlays = with allSources; [
    (import ./overlay.nix)
  ];

  overlays = sourcesOverlays;
  nixpkgs-overlayed = mkPkgs { inherit overlays; };

in if builtins.pathExists ./packages.nix then
  (import ./packages.nix) nixpkgs-overlayed
else {
  inherit (nixpkgs-overlayed.haskellPackages) prelude-polysemy;
}
