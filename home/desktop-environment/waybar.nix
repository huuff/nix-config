{ pkgs, lib, config, osConfig, ... }:
let
  hasHomePartition = osConfig.fileSystems ? "/home";
in
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      topBar = {
        layer = "top";
        position = "top";
        height = 35;

        modules-left = [ "custom/notification" "hyprland/workspaces" "group/hardware"];
        modules-center = ["clock"];
        modules-right = [ "custom/mullvad" "network" "battery" "pulseaudio" "hyprland/language" "tray"];

        # stolen from https://haseebmajid.dev/posts/2024-03-15-til-how-to-get-swaync-to-play-nice-with-waybar/
        "custom/notification" = {
          tooltip = false;
          format = "{icon}<sub> {text}</sub>";
          format-icons = {
            notification = "󱅫";
            none = "";
            dnd-notification = " ";
            dnd-none = "󰂛";
            inhibited-notification = " ";
            inhibited-none = "";
            dnd-inhibited-notification = " ";
            dnd-inhibited-none = " ";
          };
          return-type = "json";
          exec-if = "which swaync-client";
          exec = "swaync-client -swb";
          on-click = "sleep 0.1 && swaync-client -t -sw";
          on-click-right = "sleep 0.1 && swaync-client -d -sw";
          escape = true;
        };

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
            "tor browser" = "";
          };
        };

        clock = {
          format = "<span size='x-small'>{0:%A, %d %B %Y}</span>\n<b>{0:%H:%M}</b>";
          justify = "center";
        };

        "custom/mullvad" = {
          exec = lib.getExe (pkgs.writeShellApplication {
            name = "mullvad-waybar";
            runtimeInputs = with pkgs; [ mullvad jq jo ];
            text = ''
              status=$(mullvad status --json)
              location="$(echo "$status" | jq -r '"\(.details.location.country)"')"
              state="$(echo "$status" | jq -r .state)"

              if [ "$state" = "connected" ]; then
                jo text="󰦝 $location" class=connected
              else
                jo text="󰦞 $location" class=disconnected
              fi
            '';
          });
          interval = 5;
          tooltip-format = "Mullvad VPN";
          max-length = 10;
          return-type = "json";
        };

        network = {
          format-wifi = "{icon} {essid}";
          format-ethernet = "󰈀 Wired";
          format-disconnected = "󰖪 No connection";
          format-icons = ["󰤯" "󰤟" "󰤢" "󰤥" "󰤨"];
          max-length = 15;
          tooltip-format = "{essid}({ifname}): {ipaddr}/{cidr}";
        };

        "hyprland/language" = {
          format = "󰌌 {}";
          format-en = "en";
          format-es = "es";
          on-click = "hyprctl switchxkblayout video-bus next";
        };

        battery = {
          format = "{icon} {capacity}%";
          format-icons = ["" "" "" "" ""];
          format-charging = " {capacity}%";
          format-plugged = " {capacity}%";
          states = {
            warning = 30;
            critical = 15;
          };
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
          format = "󰙅<sub> {percentage_used}%</sub>";
          states = {
            warning = 75;
            critical = 90;
          };
        };

        "disk#home" = lib.mkIf hasHomePartition {
          path = "/home";
          format = "󰋜<sub> {percentage_used}%</sub>";
          unit = "GB";
          states = {
            warning = 75;
            critical = 90;
          };
        };

        cpu = {
          format = "󰘚<sub> {usage}%</sub>";
          tooltip = true;
          states = {
            warning = 75;
            critical = 90;
          };
        };

        memory = {
          format = "󰍛<sub> {percentage}%</sub>";
          states = {
            warning = 75;
            critical = 90;
          };
        };

        temperature = {
          format = "{icon}<sub> {temperatureC}°C</sub>";
          format-icons = ["" "" "" "" ""];
        };

        "group/hardware" = {
          orientation = "horizontal";
          modules = ["temperature" "cpu" "memory" "disk#root"] ++ (lib.optional hasHomePartition "disk#home");
        };

        tray = {
          spacing = 10;
        };
      };
    };

    style = lib.mkAfter ''
      window#waybar {
        background-color: rgba(0, 0, 0, 0);
        color: #${config.lib.stylix.colors.base05};
      }

      #custom-notification, #hardware, #workspaces, #clock, #custom-mullvad, #network, #battery, #pulseaudio, #language, #tray {
        background-color: #${config.lib.stylix.colors.base01};
        opacity: 0.75;
        border-radius: 6px;
        padding: 0 10px;
        margin: 0 3px;
      }

      #custom-mullvad.connected {
        color: #${config.lib.stylix.colors.base0B};
      }

      .warning {
        color: #${config.lib.stylix.colors.base09};
      }

      .critical {
        color: #${config.lib.stylix.colors.base08};
      }

      #workspaces {
        padding: 0;
      }

      #workspaces button {
        border: none;
      }

      #workspaces button.visible {
        border-bottom: 3px solid #${config.lib.stylix.colors.base05};
      }

      #workspaces button:hover {
        background-color: #${config.lib.stylix.colors.base02};
      }

      #workspaces button.urgent {
        background-color: #${config.lib.stylix.colors.base09};
        color: #${config.lib.stylix.colors.base00};
      }
    '';
  };
}
