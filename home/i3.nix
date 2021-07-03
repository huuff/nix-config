{ pkgs, ... }:
{
  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config.modifier = "Mod4";
      config.window.titlebar = false;
      config.terminal = "st";
      config.menu = "rofi -show run";
    };
  };

  programs.rofi = {
    enable = true;
    theme = "dmenu";
  };

  home.packages = with pkgs; [
    i3status
  ];
}
