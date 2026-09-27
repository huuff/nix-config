{ pkgs, lib, ... }:
{
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
          timeout = 180; # 3 minutes
          on-timeout = "${lib.getExe' pkgs.systemd "loginctl"} lock-session";
        }
        {
          timeout = 300; # 5 minutes
          on-timeout = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms off";
          on-resume = "${lib.getExe' pkgs.hyprland "hyprctl"} dispatch dpms on";
        }
        {
          timeout = 600; # 10 minutes
          on-timeout = "${lib.getExe' pkgs.systemd "systemctl"} hibernate";
        }
      ];
    };
  };

  # keeps screen on when playing on fullscreen
  services.caffeine.enable = true;
  # otherwise it doesn't work with UWSM
  systemd.user.services.caffeine.Unit.After = [ "graphical-session.target" ];

}
