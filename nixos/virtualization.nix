{ pkgs, config, lib, user, ... }:
with lib;
{
  config = {
    # I'm not sure whether this works, I have like 3 different ways of setting groups
    # in my previous files. Check it out.
    users.users.${user}.extraGroups = [ "docker" "libvirtd" "vboxusers" ];

    virtualisation = {
      virtualbox.host = {
        enable = true;
        # XXX: I'm having problems with VBox 7, so I just copy-pasted the previous derivation
        # I should eventually remove this and TODO maybe put it only for office
        # Also, this compiles the whole package lol
        package = pkgs.libsForQt5.callPackage ./virtualbox-6 {
          stdenv = pkgs.stdenv_32bit;
          inherit (pkgs.gnome2) libIDL;
          jdk = pkgs.openjdk8; # TODO: remove override https://github.com/NixOS/nixpkgs/pull/89731
        };
      };
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
