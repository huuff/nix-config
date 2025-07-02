{ pkgs, ... }:
{

  services.flatpak.enable = true;

  # necessary for flatpak
  xdg.portal = {
    enable = true;  
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk";
  };

}
