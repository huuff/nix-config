{ pkgs, lib, ... }:
{
  home.packages = [
    pkgs.nerd-fonts.fira-code
  ];

  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      topBar = {
        layer = "top";
        position = "top";
        height = 25;

        modules-left = [ "hyprland/workspaces" "cpu" "memory" "disk#root" "disk#home" ];
        modules-right = [ "custom/mullvad" "network" "battery" "pulseaudio" "hyprland/language" "tray"];
        modules-center = [ "clock" ];

        clock = {
          format = "{0:%A, %d %B %Y}\n<b>{0:%H:%M}</b>";
          justify = "center";
        };

        "custom/mullvad" = {
          exec = lib.getExe (pkgs.writeShellApplication {
            name = "mullvad-waybar";
            runtimeInputs = with pkgs; [ mullvad jq ];
            text = ''
              status=$(mullvad status --json)
              location="$(echo "$status" | jq -r '"\(.details.location.country)"')"
              state="$(echo "$status" | jq -r .state)"

              if [ "$state" = "connected" ]; then
                echo "󰦝 $location"
              else
                echo "󰦞 $location"
              fi
            '';
          });
          interval = 5;
          tooltip-format = "Mullvad VPN";
          max-length = 10;
        };

        network = {
          format-wifi = "{icon} {essid}";
          format-ethernet = "󰈀";
          format-disconnected = "󰖪";
          format-icons = ["󰤯" "󰤟" "󰤢" "󰤥" "󰤨"];
          max-length = 10;
          tooltip-format = "{essid}({ifname}): {ipaddr}/{cidr}";
        };

        "hyprland/language" = {
          format = "⌨ {}";
          format-en = "en";
          format-es = "es";
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

        "disk#root" = {
          path = "/";
          format = "󰙅 {percentage_used}%";
        };

        "disk#home" = {
          path = "/home";
          format = "󰋜 {percentage_used}%";
          unit = "GB";
        };

        cpu = {
          format = "󰘚 {usage}%";
          tooltip = true;
        };

        memory = {
          format = "󰍛 {percentage}%";
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
        background-color: rgba(40, 40, 40, 0.6);
        color: #ebdbb2;
      }

      #clock,
      #custom-mullvad,
      #network,
      #language,
      #cpu,
      #memory,
      #battery,
      #disk,
      #pulseaudio,
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
