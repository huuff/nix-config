{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    libinput = {
      enable = true;
      touchpad.scrollMethod = "edge";
    };
    displayManager = {
      lightdm.enable = true;
      defaultSession = "xsession";
      session = [{
          manage = "desktop";
          name = "xsession";
          start = ''exec $HOME/.xsession'';
      }];
    };

    desktopManager = {
      xterm.enable = false;
    };

    layout = "es";
    xkbOptions = "eurosign:e";
  };
}
