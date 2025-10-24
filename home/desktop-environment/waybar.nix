{ pkgs, lib, ... }:
{
  home.packages = [
    pkgs.nerd-fonts.fira-code
  ];

  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      # this bar is separated from the top bar so that its larger height
      # doesn't also enlarge the main one
      clock = {
        layer = "top";
        position = "top";
        margin-top = -30;
        height = 35;
        exclusive = false;
        mode = "overlay";

        modules-center = ["clock"];

        clock = {
          format = "<span size='x-small'>{0:%A, %d %B %Y}</span>\n<b>{0:%H:%M}</b>";
          justify = "center";
        };
      };

      topBar = {
        layer = "top";
        position = "top";
        height = 25;

        modules-left = [ "hyprland/workspaces" "group/hardware"];
        modules-right = [ "custom/mullvad" "network" "battery" "pulseaudio" "hyprland/language" "tray"];

        "hyprland/workspaces" = {
          format = "<sub>{icon}</sub> {windows}";
          format-window-separator = " ";
          window-rewrite-default = "?";
          window-rewrite = {
            "firefox" = "";
            "code" = "󰨞";
            "slack" = "󰒱";
            "alacritty" = "";
            "chromium" = "";
          };
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
          max-length = 15;
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

        "group/hardware" = {
          orientation = "horizontal";
          modules = ["cpu" "memory" "disk#root" "disk#home"];
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
        background-color: rgba(0, 0, 0, 0);
        color: #ebdbb2;
      }

      /* HARDWARE GROUP */
      #memory, #cpu, #disk {
        margin: 0 2px;
      }


      #hardware, #workspaces, #clock, #custom-mullvad, #network, #battery, #pulseaudio, #language, #tray {
        background-color: rgba(70, 70, 60, 0.75);
        border-radius: 6px;
        padding: 0 10px;
        margin: 0 5px;
      }

      #workspaces {
        margin-left: 5px;
        margin-right: 25px;
        padding: 0;
      }

      #workspaces button {
        border: none;
      }

      #workspaces button.visible {
        border-bottom: 3px solid white;   
      }

      #workspaces button:hover {
        background-color: #3c3836;
      }
    '';
  };
}
