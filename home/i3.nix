# TODO: This isn't really just about i3, there's also rofi and screen-locking
{ pkgs, ... }:
{
  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config = {
        modifier = "Mod4";
        window.titlebar = false;
        terminal = "st";
        menu = "rofi -show run";
      };
    };
  };

  programs.rofi = {
    enable = true;
    theme = "dmenu";
  };

  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.i3lock}/bin/i3lock -n -c 000000";
    inactiveInterval = 10;
  };

  home.packages = with pkgs; [
    i3lock
    i3status
  ];
}
