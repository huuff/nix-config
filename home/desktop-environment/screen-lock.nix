# TODO: Should this be in the "desktop-environment" directory?
{ pkgs,  ... }:
{
  # TODO: move the caffeine config here?
  services.screen-locker = {
    enable = true;
    lockCmd = (import ../commands.nix { inherit pkgs; }).lock;
    inactiveInterval = 5;
  };
}
