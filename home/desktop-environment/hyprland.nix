{ pkgs, lib, ... }:
{
  wayland.windowManager.hyprland = {
    enable = true;
    configType = "lua";
    # XXX: conflicts with nixos-level UWSM (see https://wiki.hypr.land/Useful-Utilities/Systemd-start/#uwsm)
    systemd.enable = false;
    plugins = [ ];

    settings =
      let
        toLua = lib.generators.toLua { };
        inline = lib.generators.mkLuaInline;
        mod = "SUPER";
        bind = keys: dispatcher: opts: {
          _args = [
            keys
            (inline dispatcher)
          ]
          ++ lib.optional (opts != { }) opts;
        };
        exec = cmd: "hl.dsp.exec_cmd(${toLua cmd})";
        swayosd = args: exec "${lib.getExe' pkgs.swayosd "swayosd-client"} ${args}";
        workspaces = lib.genList (i: i + 1) 10;
        # 10 maps to key 0
        wsKey = i: toString (lib.mod i 10);
      in
      {
        config = {
          input = {
            kb_layout = "us,es";
          };

          decoration = {
            blur = {
              enabled = true;
              size = 8;
              passes = 3;
            };
          };

          general = {
            allow_tearing = false;
            # bottom lets windows tuck slightly behind the docked chips
            gaps_out = {
              top = 12;
              right = 10;
              bottom = 20;
              left = 10;
            };
          };
        };

        bind = [
          # Layout toggle on every keyboard at once — Hyprland layouts are
          # per-device, so XKB's grp:alt_shift_toggle desyncs multiple keyboards
          # Keep the modifier events visible to applications. Otherwise Slack's
          # Electron shell sees a lone Alt press and moves focus out of the editor.
          # all four side combos; the pressed mod is not yet in the mask at match time
          (bind "ALT + Shift_L" (exec "hyprctl switchxkblayout all next") { non_consuming = true; })
          (bind "ALT + Shift_R" (exec "hyprctl switchxkblayout all next") { non_consuming = true; })
          (bind "SHIFT + Alt_L" (exec "hyprctl switchxkblayout all next") { non_consuming = true; })
          (bind "SHIFT + Alt_R" (exec "hyprctl switchxkblayout all next") { non_consuming = true; })

          # Terminal
          (bind "${mod} + Return" (exec "${lib.getExe pkgs.app2unit} -- ${lib.getExe pkgs.ghostty}") { })

          # App launcher
          # TODO: can't use the one from pkgs because it's different from the one that I get from the walker
          # flake! so this depends on it being in scope, I need something better
          (bind "${mod} + d" (exec "walker") { })

          # Focus controls (i3-style with vim keys)
          (bind "${mod} + h" ''hl.dsp.focus({ direction = "left" })'' { })
          (bind "${mod} + j" ''hl.dsp.focus({ direction = "down" })'' { })
          (bind "${mod} + k" ''hl.dsp.focus({ direction = "up" })'' { })
          (bind "${mod} + l" ''hl.dsp.focus({ direction = "right" })'' { })

          # Move windows (i3-style with vim keys)
          (bind "${mod} + SHIFT + h" ''hl.dsp.window.move({ direction = "left" })'' { })
          (bind "${mod} + SHIFT + j" ''hl.dsp.window.move({ direction = "down" })'' { })
          (bind "${mod} + SHIFT + k" ''hl.dsp.window.move({ direction = "up" })'' { })
          (bind "${mod} + SHIFT + l" ''hl.dsp.window.move({ direction = "right" })'' { })

          # Fullscreen (i3-style)
          (bind "${mod} + f" "hl.dsp.window.fullscreen()" { })

          # Toggle floating
          (bind "${mod} + Space" ''hl.dsp.window.float({ action = "toggle" })'' { })

          # Close window
          (bind "${mod} + SHIFT + q" "hl.dsp.window.close()" { })

          # Mouse bindings for floating windows
          (bind "${mod} + mouse:272" "hl.dsp.window.drag()" { mouse = true; })
          (bind "${mod} + mouse:273" "hl.dsp.window.resize()" { mouse = true; })

          # Media controls with an on-screen indicator from SwayOSD. Locked
          # bindings work while the session is locked; repeating bindings allow
          # volume keys to be held down.
          (bind "XF86AudioPlay" (swayosd "--playerctl play-pause") { locked = true; })
          (bind "XF86AudioPause" (swayosd "--playerctl pause") { locked = true; })
          (bind "XF86AudioNext" (swayosd "--playerctl next") { locked = true; })
          (bind "XF86AudioPrev" (swayosd "--playerctl prev") { locked = true; })
          (bind "XF86AudioStop" (swayosd "--playerctl stop") { locked = true; })
          (bind "XF86AudioMute" (swayosd "--output-volume mute-toggle") { locked = true; })
          (bind "XF86AudioMicMute" (swayosd "--input-volume mute-toggle") { locked = true; })
          (bind "XF86AudioRaiseVolume" (swayosd "--output-volume +5 --max-volume 100") {
            locked = true;
            repeating = true;
          })
          (bind "XF86AudioLowerVolume" (swayosd "--output-volume -5") {
            locked = true;
            repeating = true;
          })
        ]
        # Workspace switching and moving windows to workspaces (i3-style)
        ++ map (i: bind "${mod} + ${wsKey i}" "hl.dsp.focus({ workspace = ${toString i} })" { }) workspaces
        ++ map (
          i: bind "${mod} + SHIFT + ${wsKey i}" "hl.dsp.window.move({ workspace = ${toString i} })" { }
        ) workspaces;

        window_rule = [
          {
            match.class = "^(Emulator)$";
            float = true;
          }
        ];

        layer_rule = [
          {
            match.namespace = "waybar";
            blur = true;
            ignore_alpha = 0.3;
          }
        ];
      };
  };
}
