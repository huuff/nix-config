# TODO: Should this be in the "desktop-environment" directory?
{ pkgs,  ... }:
{
  services.screen-locker = {
    enable = true;
    lockCmd = (import ../commands.nix { inherit pkgs; }).lock;
    inactiveInterval = 60;
  };
}
