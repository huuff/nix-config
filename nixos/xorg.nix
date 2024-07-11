{ ... }:
{
  services = {
    xserver = {
      desktopManager = {
        xterm.enable = false;
      };

      xkb = {
        layout = "es";
        options = "eurosign:e";
      };
      enable = true;

      displayManager = {
        lightdm.enable = true;
        startx.enable = true;
        defaultSession = "xsession";
        session = [{
          manage = "desktop";
          name = "xsession";
          start = ''exec $HOME/.xsession'';
        }];
      };
    };

    libinput = {
      enable = true;
      touchpad.scrollMethod = "edge";
    };

  };
}
