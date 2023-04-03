{ pkgs, lib, ... }:
with lib;
let
  lockCommand = "${pkgs.i3lock}/bin/i3lock --nofork --color 000000";
in
{
  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config = rec {
        modifier = "Mod4";
        window.titlebar = false;
        terminal = "alacritty";
        menu = "rofi -show run";
        keybindings = mkOptionDefault {
          "${modifier}+F4" = "exec ${lockCommand}"; 
        };
      };
    };
  };

  programs.i3status = {
     enable = true;

      general = {
        colors = true;
        interval = 5;
      };

      modules = {
        ipv6 = { position = 1; };

        "wireless _first_" = {
          position = 2;
          settings = {
            format_up = "W: (%quality at %essid) %ip";
            format_down = "W: down";
          };
        };

        "ethernet _first_" = {
          position = 3;
          settings = {
            format_up = "E: %ip (%speed)";
            format_down = "E: down";
          };
        };

        "battery all" = {
          position = 4;
          settings = { format = "%status %percentage %remaining"; };
        };

        "volume master" = {
            position = 5;
            settings = {
              format = "♪ %volume";
              format_muted = "♪ muted (%volume)";
              device = "pulse:0";
            };
        };

        "disk /" = {
          position = 6;
          settings = { format = "%avail"; };
        };

        load = {
          position = 7;
          settings = { format = "%1min"; };
        };

        memory = {
          position = 8;
          settings = {
            format = "%used | %available";
            threshold_degraded = "1G";
            format_degraded = "MEMORY < %available";
          };
        };

        "tztime local" = {
          position = 9;
          settings = { format = "%Y-%m-%d %H:%M:%S"; };
        };
      };
  };

  programs.rofi = {
    enable = true;
    theme = ./rofi-dmenu-theme.rasi;
  };

  services.screen-locker = {
    enable = true;
    lockCmd = lockCommand;
    inactiveInterval = 5;
  };
}
