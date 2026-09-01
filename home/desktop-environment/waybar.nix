{
  pkgs,
  lib,
  config,
  osConfig,
  ...
}:
let
  hasHomePartition = osConfig.fileSystems ? "/home";
in
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    settings = {
      topBar = {
        name = "topBar";
        layer = "top";
        position = "top";
        height = 30;
        margin-top = 4;
        margin-left = 14;
        margin-right = 14;

        modules-left = [ "hyprland/workspaces" ];
        modules-center = [ "group/islandCenter" ];
        modules-right = [ "group/islandRight" ];

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
            "class<[^>]*code[^>]*>" = "󰨞";
            "slack" = "󰒱";
            "ghostty" = "";
            "orca" = "󱚝";
            "chromium" = "";
            "tor browser" = "";
            "jetbrains-idea" = "";
            "ledger-wallet" = "";
            "hoppscotch" = "󰖟";
            "Jellyfin" = "󰎁";
            "pavucontrol" = "󰕾";
            "org.remmina.Remmina" = "󰢹";
            "emulator" = "󰀲";
            "monero-core" = "ɱ";
            "libreoffice-startcenter" = "󰏆";
            "libreoffice-writer" = "󰈬";
            "libreoffice-calc" = "󰈛";
            "libreoffice-impress" = "󰈧";
            "libreoffice-draw" = "󰽉";
            "libreoffice-base" = "󰆼";
            "libreoffice-math" = "󰪚";
          };
          # TODO: this is coupled to my laptop so not perfect
          persistent-workspaces = {
            "eDP-1" = [
              1
              3
              5
              7
              9
            ];
            "DP-1" = [
              2
              4
              6
              8
              10
            ];
          };
        };

        clock = {
          format = "<b>{0:%H:%M}</b> <span size='small'>{0:%a %d %b}</span>";
        };

        "custom/mullvad" = {
          exec = lib.getExe (
            pkgs.writeShellApplication {
              name = "mullvad-waybar";
              runtimeInputs = with pkgs; [
                mullvad
                jq
                jo
              ];
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
            }
          );
          interval = 5;
          tooltip-format = "Mullvad VPN";
          max-length = 10;
          return-type = "json";
        };

        "custom/tailscale" = {
          exec = lib.getExe (
            pkgs.writers.writeNuBin "tailscale-waybar" ''
              let result = (^${lib.getExe pkgs.tailscale} status --json | complete)
              let status = if $result.exit_code == 0 { $result.stdout | from json } else { {} }

              let backend = ($status.BackendState? | default "Stopped")

              if $backend == "Running" {
                let tailnet = ($status.CurrentTailnet?.Name? | default "unknown")
                let exit_node_id = ($status.ExitNodeStatus?.ID? | default "")

                if ($exit_node_id | is-empty) {
                  {text: $"󰛳 ($tailnet)", tooltip: $"Tailnet: ($tailnet)", class: "connected"} | to json -r
                } else {
                  let exit_node_host = (
                    $status.Peer?
                    | default {}
                    | values
                    | where ID == $exit_node_id
                    | get HostName?
                    | get 0?
                    | default "?"
                  )
                  {text: $"󰛳 ($tailnet) → ($exit_node_host)", tooltip: $"Tailnet: ($tailnet)\nExit node: ($exit_node_host)", class: "exit-node"} | to json -r
                }
              } else {
                {text: "󰛳 Offline", tooltip: $"Tailscale: ($backend)", class: "disconnected"} | to json -r
              }
            ''
          );
          interval = 5;
          max-length = 30;
          return-type = "json";
        };

        network = {
          format-wifi = "{icon} {essid}";
          format-ethernet = "󰈀 Wired";
          format-disconnected = "󰖪 Disconnected";
          format-icons = [
            "󰤯"
            "󰤟"
            "󰤢"
            "󰤥"
            "󰤨"
          ];
          max-length = 15;
          tooltip-format = "{essid}({ifname}): {ipaddr}/{cidr}";
        };

        "hyprland/language" = {
          format = "󰌌 {}";
          format-en = "en";
          format-es = "es";
          on-click = "hyprctl switchxkblayout all next";
        };

        battery = {
          format = "{icon} {capacity}%";
          format-icons = [
            ""
            ""
            ""
            ""
            ""
          ];
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
            default = [
              "󰕿"
              "󰖀"
              "󰕾"
            ];
          };
          on-click = "pavucontrol";
        };

        "group/islandCenter" = {
          orientation = "horizontal";
          modules = [
            "clock"
            "custom/notification"
          ];
        };

        "group/islandRight" = {
          orientation = "horizontal";
          modules = [
            "custom/mullvad"
            "custom/tailscale"
            "network"
            "pulseaudio"
            "battery"
            "hyprland/language"
          ];
        };

      };

      # docked corner chips: vitals bottom-left, tray bottom-right; windows slide behind
      bottomBar = {
        name = "bottomBar";
        layer = "top";
        position = "bottom";
        exclusive = false;
        height = 30;
        margin-bottom = 0;
        margin-left = 0;
        margin-right = 0;

        modules-left = [ "group/vitals" ];
        modules-right = [ "tray" ];

        "group/vitals" = {
          orientation = "horizontal";
          modules = [
            "temperature"
            "cpu"
            "memory"
            "disk#root"
          ]
          ++ (lib.optional hasHomePartition "disk#home");
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
          format-icons = [
            ""
            ""
            ""
            ""
            ""
          ];
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

      /* the three glass islands */
      #workspaces, #islandCenter, #islandRight {
        background-color: alpha(#${config.lib.stylix.colors.base00}, 0.68);
        border: 1px solid alpha(#${config.lib.stylix.colors.base05}, 0.14);
        border-radius: 15px;
        padding: 0 6px;
      }

      /* bottom corner chips */
      #vitals, window#waybar.bottomBar #tray {
        background-color: alpha(#${config.lib.stylix.colors.base00}, 0.85);
        border-top: 1px solid alpha(#${config.lib.stylix.colors.base05}, 0.14);
        padding: 0 12px;
      }

      #vitals {
        border-right: 1px solid alpha(#${config.lib.stylix.colors.base05}, 0.14);
        border-radius: 0 12px 0 0;
      }

      window#waybar.bottomBar #tray {
        border-left: 1px solid alpha(#${config.lib.stylix.colors.base05}, 0.14);
        border-radius: 12px 0 0 0;
      }

      #custom-notification, #clock, #custom-mullvad, #custom-tailscale, #network,
      #battery, #pulseaudio, #language, #tray {
        padding: 0 8px;
      }

      #temperature, #cpu, #memory, #disk {
        padding: 0 3px;
      }

      #clock {
        border-right: 1px solid alpha(#${config.lib.stylix.colors.base05}, 0.18);
        margin: 7px 4px;
        padding: 0 12px;
      }

      #custom-mullvad.connected, #custom-tailscale.connected {
        color: #${config.lib.stylix.colors.base0B};
      }

      #custom-tailscale.exit-node {
        color: #${config.lib.stylix.colors.base09};
        font-weight: bold;
      }

      #custom-tailscale.disconnected {
        color: #${config.lib.stylix.colors.base03};
      }

      .warning {
        color: #${config.lib.stylix.colors.base09};
      }

      .critical {
        color: #${config.lib.stylix.colors.base08};
      }

      #network.disconnected {
        color: #${config.lib.stylix.colors.base08};
      }

      /* .modules-left prefix + full border shorthand: stylix ships
         .modules-left #workspaces button.active { border-bottom: 3px solid },
         which outranks bare #workspaces selectors */
      .modules-left #workspaces button {
        border: 1px solid transparent;
        border-radius: 12px;
        padding: 0 10px;
        margin: 3px 1px;
        color: #${config.lib.stylix.colors.base03};
      }

      .modules-left #workspaces button.active {
        background-color: alpha(#${config.lib.stylix.colors.base09}, 0.18);
        border: 1px solid alpha(#${config.lib.stylix.colors.base09}, 0.55);
        color: #${config.lib.stylix.colors.base05};
      }

      .modules-left #workspaces button.visible {
        border: 1px solid alpha(#${config.lib.stylix.colors.base05}, 0.25);
        color: #${config.lib.stylix.colors.base05};
      }

      .modules-left #workspaces button:hover {
        background-color: #${config.lib.stylix.colors.base02};
      }

      .modules-left #workspaces button.urgent {
        background-color: #${config.lib.stylix.colors.base09};
        border: 1px solid transparent;
        color: #${config.lib.stylix.colors.base00};
      }
    '';
  };
}
