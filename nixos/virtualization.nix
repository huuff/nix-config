{ pkgs, config, lib, user, ... }:
with lib;
{
  config = {
    # I'm not sure whether this works, I have like 3 different ways of setting groups
    # in my previous files. Check it out.
    users.users.${user}.extraGroups = [ "docker" "libvirtd" "vboxusers" ];

    virtualisation = {
      # TODO: Enable UID remapping!
      # TODO: Actually, remove it when I fix my usages that depend on root (mainly, COPY instructions) and use podman
      docker = {
        enable = true;
        autoPrune.enable = true;
      };
      libvirtd.enable = true;
      podman.enable = true;
    };

    # XXX: NFS port? I'm not sure why I did this and might be dangerous.
    # Likely some app I used needed sharing a volume between a VM and host through NFS.
    networking = {
      firewall = {
        extraCommands = ''
          ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
        '';
        allowedTCPPorts = [ 2049 ];
        checkReversePath = false;

      };
    };
  };

}
