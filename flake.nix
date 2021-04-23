{
  description = "Cardano Node";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # Would rather use iohkNix/nixpkgs here but we may need to add a flake there first
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    iohkNix.url = "github:input-output-hk/iohk-nix/flake";
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, ... }:
    let
      inherit (haskellNix.internal) config;
      overlays = with iohkNix.overlays; [
        haskellNix.overlay
        haskell-nix-extra
        crypto
        cardano-lib
        (final: prev: {
          gitrev = self.rev or "dirty";
          commonLib = final.lib // final.cardano-lib;
        })
        (import ./nix/pkgs.nix)
      ];
    in utils.lib.eachSystem (import ./supported-systems.nix) (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };
        inherit (pkgs.lib) systems mapAttrs' nameValuePair recursiveUpdate;
        windowsPkgs = import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.mingwW64;
        };
        flake = pkgs.cardanoNodeProject.flake {};
        windowsFlake = windowsPkgs.cardanoNodeProject.flake {};
        prefixNamesWith = p: mapAttrs' (n: v: nameValuePair "${p}${n}" v);
      in recursiveUpdate flake {

        packages = prefixNamesWith "windows:" windowsFlake.packages;
        checks = prefixNamesWith "windows:" windowsFlake.checks;

        # Built by `nix build .`
        defaultPackage = flake.packages."cardano-node:exe:cardano-node";

        # This is used by `nix develop .` to open a devShell
        devShell = import ./shell.nix { inherit pkgs; };

        apps.repl = utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };
      }
    );
}
