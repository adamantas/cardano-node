# our packages overlay
final: prev: with final;
  let
    compiler = config.haskellNix.compiler or "ghc8104";
  in {
  cardanoNodeProject = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  cardanoNodeHaskellPackages = cardanoNodeProject.hsPkgs;
  cardanoNodeProfiledHaskellPackages = (import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    profiling = true;
  }).hsPkgs;
  cardanoNodeEventlogHaskellPackages = (import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    eventlog = true;
  }).hsPkgs;
  cardanoNodeAssertedHaskellPackages = (import ./haskell.nix {
    inherit config
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    assertedPackages = [
      "ouroboros-consensus"
      "ouroboros-consensus-cardano"
      "ouroboros-consensus-byron"
      "ouroboros-consensus-shelley"
      "ouroboros-network"
      "network-mux"
    ];
  }).hsPkgs;

  #Grab the executable component of our package.
  inherit (cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node;
  inherit (cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (cardanoNodeHaskellPackages.cardano-topology.components.exes) cardano-topology;
  inherit (cardanoNodeHaskellPackages.bech32.components.exes) bech32;
  inherit (cardanoNodeHaskellPackages.cardano-submit-api.components.exes) cardano-submit-api;
  cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-eventlogged = cardanoNodeEventlogHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-asserted = cardanoNodeAssertedHaskellPackages.cardano-node.components.exes.cardano-node;

  # expose the db-converter and cardano-ping from the ouroboros-network we depend on
  inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;
  inherit (cardanoNodeHaskellPackages.network-mux.components.exes) cardano-ping;

  mkCluster = cfg: callPackage ./supervisord-cluster
                     (lib.traceValSeqN 2 cfg);
  cardanolib-py = callPackage ./cardanolib-py {};

  scripts = import ./scripts.nix { inherit pkgs config customConfig; };

  clusterTests = import ./supervisord-cluster/tests { inherit pkgs; };

}
