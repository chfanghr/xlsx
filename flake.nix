{
  description = "agora-lq-staking-rewards";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" ];
    extra-substituters = [ "https://cache.iog.io" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs?rev=0cac47c9cc840f2a68cf49a6da3ff2a657ad2e64";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/v2.7.2";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { system, ... }:
        let
          pkgs = import inputs.nixpkgs-latest { inherit system; };
        in
        {
          pre-commit = {
            settings = {
              src = ./.;
              settings = { };
              hooks = {
                nixpkgs-fmt.enable = true;
                cabal-fmt.enable = true;
                fourmolu.enable = false;
                hlint.enable = false;
                statix.enable = true;
                deadnix.enable = true;
              };
            };
          };

          onchain.default = {
            src = ./.;
            ghc.version = "ghc925";
            fourmolu.package = pkgs.haskell.packages.ghc943.fourmolu_0_10_1_0;
            hlint = { };
            cabalFmt = { };
            hasktags = { };
            applyRefact = { };
            shell = { };
            hoogleImage.enable = false;
            enableBuildChecks = true;
            extraHackageDeps = [
            ];
          };
          ci.required = [ "all_onchain" ];
        };

      flake.hydraJobs.x86_64-linux =
        self.checks.x86_64-linux // self.packages.x86_64-linux;
    };
}
