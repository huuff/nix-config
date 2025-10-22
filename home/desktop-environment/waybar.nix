{ pkgs, ... }:
{
  home.packages = [
    pkgs.nerd-fonts.fira-code
  ];

  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 25;

        modules-left = [ "hyprland/workspaces" ];
        modules-right = [ "network" "hyprland/language" "battery" "pulseaudio" "clock" ];
        modules-center = [ "tray" ];

        clock = {
          format = " {:%H:%M}";
        };

        network = {
          format-wifi = "{icon} {ifname}";
          format-ethernet = "󰈀 {ifname}";
          format-disconnected = "󰖪";
          format-icons = ["󰤯" "󰤟" "󰤢" "󰤥" "󰤨"];
          tooltip-format = "{ifname}: {ipaddr}/{cidr}";
        };

        "hyprland/language" = {
          format = "⌨ {}";
          on-click = "hyprctl switchxkblayout video-bus next";
        };

        battery = {
          format = "{icon} {capacity}%";
          format-icons = ["" "" "" "" ""];
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-muted = "󰖁 {volume}%";
          format-icons = {
            default = ["󰕿" "󰖀" "󰕾"];
          };
          on-click = "pavucontrol";
        };

        tray = {
          spacing = 10;
        };
      };
    };

    style = ''
      * {
        font-family: "FiraCode Nerd Font", monospace;
        font-size: 13px;
      }

      window#waybar {
        background-color: rgba(40, 40, 40, 0.4);
        color: #ebdbb2;
      }

      #clock {
        padding: 0 10px;
      }

      #network {
        padding: 0 10px;
      }

      #language {
        padding: 0 10px;
      }

      #battery {
        padding: 0 10px;
      }

      #pulseaudio {
        padding: 0 10px;
      }

      #tray {
        padding: 0 10px;
      }

      #workspaces {
        margin: 0 5px;
      }

      #workspaces button {
        padding: 0 8px;
        background-color: transparent;
        color: #ebdbb2;
        border: none;
      }

      #workspaces button.visible {
        background-color: #689d6a;
        color: #282828;
      }

      #workspaces button:hover {
        background-color: #3c3836;
      }
    '';
  };
}
