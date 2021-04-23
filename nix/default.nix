{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, customConfig ? import ./custom-config.nix
, sourcesOverride ? {}
, gitrev ? null
}:
let
  sources = import ./sources.nix { inherit pkgs; } // sourcesOverride;
  haskellNix = import sources."haskell.nix" { inherit system sourcesOverride; };
  nixpkgs =
    if (sources ? nixpkgs)
    then (builtins.trace "Not using nixpkgs that haskell.nix is exposing. Use 'niv drop nixpkgs' to use haskell.nix's nixpkgs"
      sources.nixpkgs)
    else  haskellNix.sources.nixpkgs-unstable;
  iohkNix = import sources.iohk-nix { inherit system; };
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    ++ iohkNix.overlays.crypto
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {
        inherit gitrev customConfig;

        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // pkgs.iohkNix // pkgs.iohkNix.cardanoLib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources, nixpkgs and overlays
          // { inherit overlays sources nixpkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

in pkgs
