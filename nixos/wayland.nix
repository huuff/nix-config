{ pkgs, lib, ... }:
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

    programs.uwsm.enable = true;

    # XXX: this is necessary even if you the configure it with home-manager too
    programs.hyprland = {
      enable = true;
      withUWSM = true;
      xwayland.enable = true;
    };


    # XXX: this is necessary so UWSM runs on login... why doesn't `programs.uwsm.enable = true`
    # add this automatically?
    # MAYBE the sad thing is that this is coupled to zsh, maybe I should resurrect my portable shell module
    # to make it more generic
    environment.loginShellInit = let
      uwsm = lib.getExe pkgs.uwsm;
    in ''
      if ${uwsm} check may-start && ${uwsm} select; then
        exec ${uwsm} start default
      fi
    '';
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    security.pam.services.hyprlock.enable = true;
    programs.hyprlock.enable = true;
}
