{ pkgs, ... }:
{

  nix = {
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      # autofirma-nix binary cache to avoid rebuilding the Java/Spanish-admin tooling
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
    };
  };

  nixpkgs.config.allowUnfree = true;
}
