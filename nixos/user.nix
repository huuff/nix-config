{ config, pkgs, lib, user, ... }:
with lib;
{
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    # TODO: I don't really like setting the shell here... seems like it's hidden...
    # what about setting it in flake.nix and passing it as specialArgs?
    shell = pkgs.fish;
  };

  nix.settings.trusted-users = [ user ];
}
