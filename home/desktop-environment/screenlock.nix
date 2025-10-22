{ pkgs, lib, ... }:
{
  # MAYBE put here the caffeine config?

  programs.hyprlock = {
    enable = true;
  };

  services.hypridle = {
    enable = true;
    settings = {
      # An LLM wrote this, I'm not sure what it does lol
      general = {
        lock_cmd = "pidof hyprlock || ${lib.getExe pkgs.hyprlock}";
        before_sleep_cmd = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
        after_sleep_cmd = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms on";
      };

      listener = [
        {
          timeout = 300; # 5 minutes
          on-timeout = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
        }
        {
          timeout = 600; # 10 minutes
          on-timeout = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms off";
          on-resume = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms on";
        }
      ];
    };
  };
}
