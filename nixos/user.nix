{ config, lib, user, ... }:
with lib;
{
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  nix.settings.trusted-users = [ user ];
}
