{ pkgs, lib, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;
    # XXX: conflicts with nixos-level UWSM (see https://wiki.hypr.land/Useful-Utilities/Systemd-start/#uwsm)
    systemd.enable = false;
    plugins = [pkgs.hyprlandPlugins.hypr-dynamic-cursors];
    settings = {
      "$mod" = "SUPER";

      input = {
        kb_layout = "us,es";
        kb_options = "grp:alt_shift_toggle";
      };

      bind = [
        # Terminal
        "$mod, Return, exec, ${lib.getExe pkgs.app2unit} -- ${lib.getExe pkgs.alacritty}"

        # App launcher
        # TODO: can't use the one from pkgs because it's different from the one that I get from the walker
        # flake! so this depends on it being in scope, I need something better
        "$mod, d, exec, walker"

        # Focus controls (i3-style with vim keys)
        "$mod, h, movefocus, l"
        "$mod, j, movefocus, d"
        "$mod, k, movefocus, u"
        "$mod, l, movefocus, r"

        # Move windows (i3-style with vim keys)
        "$mod SHIFT, h, movewindow, l"
        "$mod SHIFT, j, movewindow, d"
        "$mod SHIFT, k, movewindow, u"
        "$mod SHIFT, l, movewindow, r"

        # Fullscreen (i3-style)
        "$mod, f, fullscreen, 0"

        # Toggle floating
        "$mod, Space, togglefloating"

        # Close window
        "$mod SHIFT, q, killactive"

        # Workspace switching (i3-style)
        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$mod, 8, workspace, 8"
        "$mod, 9, workspace, 9"
        "$mod, 0, workspace, 10"

        # Move window to workspace (i3-style)
        "$mod SHIFT, 1, movetoworkspace, 1"
        "$mod SHIFT, 2, movetoworkspace, 2"
        "$mod SHIFT, 3, movetoworkspace, 3"
        "$mod SHIFT, 4, movetoworkspace, 4"
        "$mod SHIFT, 5, movetoworkspace, 5"
        "$mod SHIFT, 6, movetoworkspace, 6"
        "$mod SHIFT, 7, movetoworkspace, 7"
        "$mod SHIFT, 8, movetoworkspace, 8"
        "$mod SHIFT, 9, movetoworkspace, 9"
        "$mod SHIFT, 0, movetoworkspace, 10"
      ];

      # Mouse bindings for floating windows
      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];

      decoration = {
        blur = {
          enabled = true;
          size = 8;
          passes = 3;
        };
      };

      general = {
        allow_tearing = false;
      };

      "plugin:dynamic-cursors" = {
        enabled = true;
        mode = "none"; # I just want shake-to-find
        shake = {
          enabled = true;
        };
      };

    };
  };
}
