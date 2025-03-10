{ pkgs, ... }:
{
  xsession = {
    enable = true;
    windowManager.i3 = {
      enable = true;
      config = {
        modifier = "Mod4";

        window.titlebar = false;

        terminal = "alacritty";

        menu = "${pkgs.rofi}/bin/rofi -show run";

        bars = [
          {
            position = "bottom";
            statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-bottom.toml";
          }
        ];
      };
    };
  };

  programs.i3status-rust = {
    enable = true;
    bars = {
      bottom = {
        blocks = [
          {
            block = "disk_space";
            path = "/";
            info_type = "available";
            interval = 60;
            warning = 20.0;
            alert = 10.0;
          }

          {
            block = "battery";
          }

          # TODO not working for some reason?
          #{
            #block = "memory";
            #format_mem = " $icon $mem_used_percents ";
            #format_swap = " $icon $swap_used_percents ";
          #}

          {
            block = "cpu";
            interval = 1;
          }

          {
            block = "load";
            interval = 1;
            format = " $icon $1m ";
          }

          { block = "sound"; }

          {
            block = "time";
            interval = 60;
            format = " $timestamp.datetime(f:'%a %d/%m %R') ";
          }
        ];
      };


    };

  };

  programs.rofi = {
    enable = true;
    theme = ./rofi-dmenu-theme.rasi;
  };

}
