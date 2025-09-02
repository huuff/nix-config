{ pkgs, ... }:
{
  hardware.bluetooth.enable = true;
  # TODO: disabled because once it's enabled, it seems to try to force me accept pairing
  # through a desktop notification, which I think it's supposed to have a button for me to click
  # and accept... only the button isn't showing for me!! Maybe it's the fault of dunst?
  #services.blueman.enable = true;

  environment.systemPackages = [
    pkgs.bluetuith # nice ncurses CLI for bluetooth device management
  ];
}
