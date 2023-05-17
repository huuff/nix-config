{ pkgs, lib, ... }:
let
  commands = import ./commands.nix { inherit pkgs; };
in
{
  services.sxhkd = {
    enable = true;
    keybindings = {
      # TODO: Also turn off screen to save battery
      "super + F4" = commands.lock;
    };
  };
}
