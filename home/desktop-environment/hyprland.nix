{ pkgs, lib, ... }:
let
  app2unit = lib.getExe pkgs.app2unit;
  ghostty = lib.getExe pkgs.ghostty;
  swayosd = lib.getExe' pkgs.swayosd "swayosd-client";
in
{
  wayland.windowManager.hyprland = {
    enable = true;
    configType = "lua";
    # XXX: conflicts with nixos-level UWSM (see https://wiki.hypr.land/Useful-Utilities/Systemd-start/#uwsm)
    systemd.enable = false;
    # HACK: hypr-dynamic-cursors is broken with Hyprland 0.54.0 (removed SCallbackInfo API)
    # https://github.com/VirtCode/hypr-dynamic-cursors/issues
    plugins = [ ];

    settings = {
      config = {
        input.kb_layout = "us,es";
        decoration.blur = {
          enabled = true;
          size = 8;
          passes = 3;
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

    extraConfig = ''
      local mod = "SUPER"
      local osd = "${swayosd}"

      -- Layout toggle on every keyboard at once — Hyprland layouts are
      -- per-device, so XKB's grp:alt_shift_toggle desyncs multiple keyboards
      -- Keep the modifier events visible to applications. Otherwise Slack's
      -- Electron shell sees a lone Alt press and moves focus out of the editor.
      -- all four side combos; the pressed mod is not yet in the mask at match time
      for _, keys in ipairs({ "ALT + Shift_L", "ALT + Shift_R", "SHIFT + Alt_L", "SHIFT + Alt_R" }) do
        hl.bind(keys, hl.dsp.exec_cmd("hyprctl switchxkblayout all next"), { non_consuming = true })
      end

      -- Terminal
      hl.bind(mod .. " + Return", hl.dsp.exec_cmd("${app2unit} -- ${ghostty}"))

      -- App launcher
      -- TODO: can't use the one from pkgs because it's different from the one that I get from the walker
      -- flake! so this depends on it being in scope, I need something better
      hl.bind(mod .. " + d", hl.dsp.exec_cmd("walker"))

      -- Fullscreen (i3-style)
      hl.bind(mod .. " + f", hl.dsp.window.fullscreen({ mode = "fullscreen" }))

      -- Toggle floating
      hl.bind(mod .. " + Space", hl.dsp.window.float())

      -- Close window
      hl.bind(mod .. " + SHIFT + q", hl.dsp.window.close())

      -- Mouse bindings for floating windows
      hl.bind(mod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
      hl.bind(mod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

      -- Focus and move windows (i3-style with vim keys)
      for key, dir in pairs({ h = "left", j = "down", k = "up", l = "right" }) do
        hl.bind(mod .. " + " .. key, hl.dsp.focus({ direction = dir }))
        hl.bind(mod .. " + SHIFT + " .. key, hl.dsp.window.move({ direction = dir }))
      end

      -- Workspace switching and moving (i3-style)
      for n = 1, 10 do
        hl.bind(mod .. " + " .. n % 10, hl.dsp.focus({ workspace = n }))
        hl.bind(mod .. " + SHIFT + " .. n % 10, hl.dsp.window.move({ workspace = n }))
      end

      -- Media controls with an on-screen indicator from SwayOSD. Locked
      -- bindings work while the session is locked; repeating bindings allow
      -- volume keys to be held down.
      for key, args in pairs({
        XF86AudioPlay = "--playerctl play-pause",
        XF86AudioPause = "--playerctl pause",
        XF86AudioNext = "--playerctl next",
        XF86AudioPrev = "--playerctl prev",
        XF86AudioStop = "--playerctl stop",
        XF86AudioMute = "--output-volume mute-toggle",
        XF86AudioMicMute = "--input-volume mute-toggle",
      }) do
        hl.bind(key, hl.dsp.exec_cmd(osd .. " " .. args), { locked = true })
      end
      hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd(osd .. " --output-volume +5 --max-volume 100"), { locked = true, repeating = true })
      hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd(osd .. " --output-volume -5"), { locked = true, repeating = true })
    '';
  };
}
