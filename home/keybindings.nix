{ pkgs, lib, ... }:
let
  commands = import ./commands.nix { inherit pkgs; };
in
{
  services.sxhkd = {
    enable = true;
    keybindings = {
      # TODO: Also turn off screen to save battery
      "super + F9" = commands.lock;
      "super + F4" = "${pkgs.brightnessctl}/bin/brightnessctl s 10-";
      "super + F5" = "${pkgs.brightnessctl}/bin/brightnessctl s 10+";
    };
  };
}
