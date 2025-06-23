{ ... }:
{
  services.xserver = {
    enable = true;
    libinput = {
      enable = true;
      touchpad.scrollMethod = "edge";
    };
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

    desktopManager = {
      xterm.enable = false;
    };

    layout = "us";
    xkb.options = "eurosign:e";
  };

  # otherwise screen-lockers don't work
  security.pam.services.xlock.enable = true;
  programs.i3lock.enable = true;
}
