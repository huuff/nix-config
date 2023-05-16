# TODO: Should this be in the "desktop-environment" directory?
{ pkgs, lib, ... }:
with lib;
let
  lockCommand = "${pkgs.i3lock}/bin/i3lock --nofork --color 000000";
in
  {
    # TODO: Use sxhkd
    # TODO: Turn off the screen also to save battery
    xsession.windowManager.i3.config.keybindings = mkOptionDefault {
      "Mod4+F4" = "exec ${lockCommand}";
    };

    services.screen-locker = {
      enable = true;
      lockCmd = lockCommand;
      inactiveInterval = 5;
    };
  }
