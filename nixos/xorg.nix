{ pkgs, ... }:
{
  services.xserver = {
    enable = true;
    libinput.enable = true;
    libinput.touchpad.scrollMethod = "edge";
    displayManager.lightdm.enable = true;
    displayManager.defaultSession = "xsession";
    displayManager.session = [
      {
        manage = "desktop";
        name = "xsession";
        start = ''exec $HOME/.xsession'';
      }
    ];

    desktopManager.xterm.enable = false;
    layout = "es";
    xkbOptions = "eurosign:e";
  };
}
