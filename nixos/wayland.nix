{ pkgs, lib, user, ... }:
{
    services.displayManager = {
      defaultSession = "hyprland";
    };

    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-hyprland
        xdg-desktop-portal-gtk
      ];
    };

    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          inherit user;
          command = "${lib.getExe pkgs.tuigreet} --time --cmd ${lib.getExe pkgs.hyprland}";
        };
      };
    };

    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };

    security.pam.services.swaylock.enable = true;
    programs.hyprlock.enable = true;
}
