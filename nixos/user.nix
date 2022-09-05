{ config, pkgs, lib, user, ... }:
with lib;
{
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
  };

  nix.settings.trusted-users = [ user ];
}
