{ pkgs, lib, user, ... }:
with lib;
{
  config = {
    # I'm not sure whether this works, I have like 3 different ways of setting groups
    # in my previous files. Check it out.
    users.users.${user}.extraGroups = [ "docker" "libvirtd" "vboxusers" ];

    # Necessary for virt-manager
    programs.dconf.enable = true;

    # Necessary for shared folders with virt-manager
    environment.systemPackages = with pkgs; [
      virtiofsd
    ];

    virtualisation = {
      # TODO: Enable UID remapping!
      # TODO: Actually, remove it when I fix my usages that depend on root (mainly, COPY instructions) and use podman
      docker = {
        enable = true;
        autoPrune.enable = true;
      };
      libvirtd.enable = true;
      podman.enable = true;

      virtualbox.host = {
        enable = true;
      };
    };

    environment.etc."vbox/networks.conf".text = ''
      * 192.168.0.0/16
    '';
  };

}
