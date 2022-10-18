{
  nixConfig.bash-prompt = "[kvstore-effectful:] ";
  description = "kvstore-effectful";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem flake-utils.lib.defaultSystems (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            kvstore-effectful =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc922";
                projectFileName = "stack.yaml";
                modules = [{
                  packages = { };
                }];
                shell.buildInputs = with pkgs; [
                  cabal-install
                  haskellPackages.cabal-fmt
                  ghcid
                  hlint
                  nixpkgs-fmt
                  stylish-haskell
                ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.kvstore-effectful.flake { };
      in
      flake
    );
}
